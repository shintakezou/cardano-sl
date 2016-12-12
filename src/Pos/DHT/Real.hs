{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-| Implementation of peer discovery using using Kademlia Distributed Hash Table.
    For more details regarding DHT see this package on hackage:
    <https://hackage.haskell.org/package/kademlia>
-}

module Pos.DHT.Real
       ( KademliaDHT (..)
       , runKademliaDHT
       , KademliaDHTConfig(..)
       , KademliaDHTInstanceConfig(..)
       , KademliaDHTInstance
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Control.Concurrent.Async.Lifted (async, mapConcurrently, race)
import           Control.Concurrent.STM          (STM, TVar, modifyTVar, newTVar,
                                                  readTVar, swapTVar, writeTVar)
import           Control.Monad.Catch             (Handler (..), MonadCatch, MonadMask,
                                                  MonadThrow, catchAll, catches, throwM,
                                                  catch)
import           Control.Monad.Morph             (hoist)
import           Control.Monad.Trans.Class       (MonadTrans)
import           Control.Monad.Trans.Control     (MonadBaseControl(..), ComposeSt,
                                                  defaultLiftBaseWith, defaultRestoreM,
                                                  MonadTransControl(..), defaultLiftWith,
                                                  defaultRestoreT)
import           Control.Monad.Base              (MonadBase(..))
import           Control.TimeWarp.Rpc            (BinaryP (..), Binding (..),
                                                  ListenerH (..), MonadDialog,
                                                  MonadResponse (..), MonadTransfer (..),
                                                  NetworkAddress, RawData (..),
                                                  TransferException (..), hoistRespCond,
                                                  listenR, sendH, sendR)
import           Control.TimeWarp.Timed          (MonadTimed, ThreadId, fork,
                                                  forkLabeled, killThread,
                                                  ms, sec)

import           Data.Binary                     (Binary, decodeOrFail, encode)
import qualified Data.ByteString                 as BS
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.Cache.LRU                  as LRU
import           Data.Hashable                   (hash)
import qualified Data.HashMap.Strict             as HM
import           Data.Text                       (Text)

import           Formatting                      (build, int, sformat, shown, (%))
import qualified Network.Kademlia                as K
import           System.Wlog                     (CanLog, HasLoggerName, WithLogger,
                                                  getLoggerName, logDebug, logError,
                                                  logInfo, logWarning, usingLoggerName)
import           Universum                       hiding (async, fromStrict, race,
                                                  mapConcurrently, toStrict)

import           Pos.DHT                         (DHTData, DHTException (..), DHTKey,
                                                  DHTMsgHeader (..), DHTNode (..),
                                                  DHTNodeType (..), DHTResponseT (..),
                                                  ListenerDHT (..), MonadDHT (..),
                                                  MonadMessageDHT (..),
                                                  MonadResponseDHT (closeResponse),
                                                  WithDefaultMsgHeader (..),
                                                  defaultSendToNeighbors,
                                                  defaultSendToNode, filterByNodeType,
                                                  joinNetworkNoThrow, randomDHTKey,
                                                  withDhtLogger)
import           Pos.Util                        (runWithRandomIntervals)
import           Debug.Trace                     (traceEvent, traceEventIO)

toBSBinary :: Binary b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => BS.ByteString -> Either [Char] (b, BS.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)

instance K.Serialize DHTData where
  toBS = toBSBinary
  fromBS = fromBSBinary

instance K.Serialize DHTKey where
  toBS = toBSBinary
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

-- | Instance of node for /Kademlia DHT/ algorithm.
data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![DHTNode]
    , kdiExplicitInitial :: !Bool
    }

-- | Node context for 'KademliaDHTInstance'.
data KademliaDHTContext m = KademliaDHTContext
    { kdcDHTInstance_         :: !KademliaDHTInstance
    , kdcAuxClosers           :: !(TVar [KademliaDHT m ()])
    , kdcListenByBinding      :: !(Binding -> KademliaDHT m (KademliaDHT m ()))
    , kdcStopped              :: !(TVar Bool)
    , kdcNoCacheMessageNames_ :: ![Text]
    }

-- | Configuration for particular 'KademliaDHTInstance'.
data KademliaDHTConfig m = KademliaDHTConfig
    { kdcPort                :: !Word16
    , kdcListeners           :: ![ListenerDHT (KademliaDHT m)]
    , kdcMessageCacheSize    :: !Int
    , kdcEnableBroadcast     :: !Bool
    , kdcNoCacheMessageNames :: ![Text]
    , kdcDHTInstance         :: !KademliaDHTInstance
    }

-- | Instance of part of config.
data KademliaDHTInstanceConfig = KademliaDHTInstanceConfig
    { kdcPort            :: !Word16
    , kdcKeyOrType       :: !(Either DHTKey DHTNodeType)
    , kdcInitialPeers    :: ![DHTNode]
    , kdcExplicitInitial :: !Bool
    }

-- | Node of /Kademlia DHT/ algorithm with access to 'KademliaDHTContext'.
newtype KademliaDHT m a = KademliaDHT { unKademliaDHT :: ReaderT (KademliaDHTContext m) m a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO,
             MonadMask, MonadTimed, MonadDialog p, CanLog, HasLoggerName)

instance MonadResponse m => MonadResponse (KademliaDHT m) where
    replyRaw dat = KademliaDHT $ replyRaw (hoist unKademliaDHT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance MonadBase IO m => MonadBase IO (KademliaDHT m) where
    liftBase = KademliaDHT . liftBase

{-
 - Can't give MonadTransControl IO KademliaDHT because its state type
 - depends upon its monad parameter. Oh well. If we really want it, we could
 - safely use unsafeCoerce I suppose.
instance MonadBaseControl IO m => MonadBaseControl IO (KademliaDHT m) where
    type StM (KademliaDHT m) a = ComposeSt KademliaDHT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM
-}

instance MonadTransfer m => MonadTransfer (KademliaDHT m) where
    sendRaw addr req = KademliaDHT $ sendRaw addr (hoist unKademliaDHT req)
    listenRaw binding sink =
        KademliaDHT $ fmap KademliaDHT $ listenRaw binding $ hoistRespCond unKademliaDHT sink
    close = lift . close

instance Applicative m => WithDefaultMsgHeader (KademliaDHT m) where
    defaultMsgHeader _ = do
        --     Caches are disabled now for non-broadcast messages
        --     uncomment lines below to enable them
        --noCacheNames <- KademliaDHT $ asks kdcNoCacheMessageNames_
        --let header =
        --        SimpleHeader . isJust . find (== messageName' msg) $
        --        noCacheNames
        let header = SimpleHeader True
        --withDhtLogger $
        --    logDebug $
        --    sformat
        --        ("Preparing message " % stext % ": header " % shown)
        --        (messageName' msg)
        --        header
        pure header

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift

type instance ThreadId (KademliaDHT m) = ThreadId m

-- | Run 'KademliaDHT' with provided 'KademliaDTHConfig'.
runKademliaDHT
    :: forall m a .
       ( WithLogger m
       , MonadIO m
       , MonadTimed m
       , MonadDialog BinaryP m
       , MonadMask m
       , MonadBaseControl IO m
       )
    => KademliaDHTConfig m -> KademliaDHT m a -> m a
runKademliaDHT kdc@(KademliaDHTConfig {..}) action =
    startDHT kdc >>= runReaderT (unKademliaDHT action')
  where
    action' =
        action''
        `catch`
        exceptionHandler
        `finally`
        (logInfo "stopping kademlia messager"
          >> stopDHT >> logInfo "kademlia messager stopped")
    action'' = do
      startMsgThread
      logDebug "running kademlia dht messager"
      joinNetworkNoThrow (kdiInitialPeers $ kdcDHTInstance)
      startRejoinThread
      action
    startRejoinThread = do
      tvar <- KademliaDHT $ asks kdcAuxClosers
      tid <- forkLabeled "Kademlia DHT rejoin thread" $ runWithRandomIntervals (ms 500) (sec 5) rejoinNetwork
      atomically $ modifyTVar tvar (killThread tid:)
    startMsgThread = do
      (tvar, listenByBinding) <-
          KademliaDHT $ (,) <$> asks kdcAuxClosers <*> asks kdcListenByBinding
      closer <- listenByBinding $ AtPort kdcPort
      atomically $ modifyTVar tvar (closer:)
    exceptionHandler :: SomeException -> KademliaDHT m a
    exceptionHandler e = do
      logError $ sformat ("runKademliaDHT : stopping due to exception " % shown) e
      throwM e

-- | Stop DHT algo.
stopDHT :: (MonadTimed m, MonadIO m, WithLogger m) => KademliaDHT m ()
stopDHT = do
    (closersTV, stoppedTV) <- KademliaDHT $ (,)
            <$> asks kdcAuxClosers
            <*> asks kdcStopped
    logInfo "stopDHT : writing stopped TVar"
    atomically $ writeTVar stoppedTV True
    logInfo "stopDHT : taking closers TVar"
    closers <- atomically $ swapTVar closersTV []
    logInfo $ sformat ("stopDHT : running " % int % " closers") (length closers)
    sequence_ closers

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaDHTInstanceConfig'.
startDHTInstance
    :: ( MonadTimed m
       , MonadIO m
       , WithLogger m
       , MonadCatch m
       , MonadBaseControl IO m
       )
    => KademliaDHTInstanceConfig -> m KademliaDHTInstance
startDHTInstance KademliaDHTInstanceConfig {..} = do
    kdiKey <- either pure randomDHTKey kdcKeyOrType
    kdiHandle <-
        (liftIO $
        K.createL
            (fromInteger . toInteger $ kdcPort)
            kdiKey
            (log' logDebug)
            (log' logError))
          `catchAll`
        (\e ->
           do logError $ sformat
                  ("Error launching kademlia at port " % int % ": " % shown) kdcPort e
              throwM e)
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    pure $ KademliaDHTInstance {..}
  where
    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText

startDHT
    :: ( MonadTimed m
       , MonadIO m
       , MonadDialog BinaryP m
       , WithLogger m
       , MonadMask m
       , MonadBaseControl IO m
       )
    => KademliaDHTConfig m -> m (KademliaDHTContext m)
startDHT KademliaDHTConfig {..} = do
    kdcStopped <- atomically $ newTVar False
    kdcAuxClosers <- atomically $ newTVar []
    msgCache <- atomically $
        newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
    let kdcListenByBinding binding = do
            closer <- listenR binding (convert <$> kdcListeners)
                (convert' $ rawListener kdcEnableBroadcast msgCache kdcStopped)
            logInfo $ sformat ("Listening on binding " % shown) binding
            return closer
    logInfo $ sformat ("Launching Kademlia, noCacheMessageNames=" % shown) kdcNoCacheMessageNames
    let kdcNoCacheMessageNames_ = kdcNoCacheMessageNames
    let kdcDHTInstance_ = kdcDHTInstance
    pure $ KademliaDHTContext {..}
  where
    convert :: ListenerDHT m -> ListenerH BinaryP DHTMsgHeader m
    convert (ListenerDHT f) = ListenerH $ \(_, m) -> getDHTResponseT $ f m
    convert' handler = getDHTResponseT . handler

-- | Return 'True' if the message should be processed, 'False' if only
--   broadcasted.
--
--   Also, broadcast any message not yet seen to the rest of the network.
rawListener
    :: ( MonadBaseControl IO m
       , MonadMask m
       , MonadDialog BinaryP m
       , MonadIO m
       , MonadTimed m
       , WithLogger m
       )
    => Bool
    -> TVar (LRU.LRU Int ())
    -> TVar Bool
    -> (DHTMsgHeader, RawData)
    -> DHTResponseT (KademliaDHT m) Bool
rawListener enableBroadcast cache kdcStopped (h, rawData@(RawData raw)) = {-# SCC rawListener #-} withDhtLogger $ do
    isStopped <- atomically $ readTVar kdcStopped
    when isStopped $ do
        closeResponse
        throwM $ FatalError "KademliaDHT stopped"
    let mHash = hash raw
    (ignoreMsg, cacheSize) <- case h of
      SimpleHeader True -> atomically $ do
                             c <- readTVar cache
                             pure (False, LRU.size c)
      _                 -> atomically $ updCache cache mHash
    if ignoreMsg
       then logInfo $
                sformat ("Ignoring message " % shown % ", hash=" % int) h mHash
       else return ()

    -- If the message is in cache, we have already broadcasted it before, no
    -- need to do it twice
    when (not ignoreMsg && enableBroadcast) $
        case h of
            BroadcastHeader -> do
              logInfo $
                sformat ("Broadcasting message " % shown % ", hash=" % int) h mHash
              lift $ sendToNetworkR rawData
            SimpleHeader _  -> pure ()
    -- If the message wasn't in the cache, we want to process it too (not
    -- simply broadcast it)
    return (not ignoreMsg)

-- | Update the cache with a hash, giving the new size of the cache, along
--   with a Bool to indicate whether the item was added (False if it was already
--   there).
updCache :: TVar (LRU.LRU Int ()) -> Int -> STM (Bool, Int)
updCache cacheTV dataHash = do
    cache <- readTVar cacheTV
    let (cache', mP) = dataHash `LRU.lookup` cache
    case mP of
      Just _ -> do
        () <- writeTVar cacheTV cache'
        pure (True, LRU.size cache')
      _      -> do
        () <- writeTVar cacheTV (LRU.insert dataHash () cache')
        pure (False, LRU.size cache')

sendToNetworkR
    :: ( MonadBaseControl IO m
       , WithLogger m
       , MonadCatch m
       , MonadIO m
       , MonadDialog BinaryP m
       , MonadTimed m
       ) => RawData -> KademliaDHT m ()
sendToNetworkR = sendToNetworkImpl sendR

sendToNetworkImpl
    :: ( MonadBaseControl IO m
       , WithLogger m
       , MonadCatch m
       , MonadIO m
       , MonadTimed m
       , MonadDialog BinaryP m
       ) => (NetworkAddress -> DHTMsgHeader -> msg -> KademliaDHT m ()) -> msg -> KademliaDHT m ()
sendToNetworkImpl sender msg = do
    logDebug "Sending message to network"
    void $ defaultSendToNeighbors seqConcurrentlyK (flip sender BroadcastHeader) msg

-- [Note: blocked indefinitely exceptions]
--
-- 'seqConcurrentlyK' is how we parallelize a bunch of sends in
-- 'sendToNeighbors'. In practice, each individual send will block on an 'STM'
-- in order to determine when its data has indeed been carried off by some
-- TCP socket, i.e. when the peer has actually read the data. If GHC determines
-- that this will block indefinitely, it won't raise the exception, because
-- 'seqConcurrentlyK' retains references to the 'ThreadId' of the blocked
-- thread, so it's possible that some other thread will 'throwTo' that id
-- and unblock the thread!
--
-- Is this causing havoc? Let's find out.
-- One option is to modify 'mapConcurrently' so that it holds only weak
-- references to the 'ThreadId's of the threads it spawns. A less invasive
-- alternative is to simply kill these threads after an arbitrary timeout.
-- So that's what we'll do.
--
seqConcurrentlyK :: MonadBaseControl IO m => [KademliaDHT m a] -> KademliaDHT m [a]
seqConcurrentlyK = KademliaDHT . mapConcurrently unKademliaDHT

instance ( MonadDialog BinaryP m
         , WithLogger m
         , MonadCatch m
         , MonadIO m
         , MonadTimed m
         , MonadBaseControl IO m
         ) => MonadMessageDHT (KademliaDHT m) where
    sendToNetwork = sendToNetworkImpl sendH
    sendToNeighbors = defaultSendToNeighbors seqConcurrentlyK sendToNode
    -- A 'sendToNode' will timeout after 30 seconds.
    -- This is achieved by racing it with a thread which just waits
    -- 30 seconds and then stops.
    sendToNode addr msg = KademliaDHT $ do
        ctx <- ask
        let worker :: m ()
            worker = flip runReaderT ctx . unKademliaDHT $ do
                defaultSendToNode addr msg
                listenOutbound >>= updateClosers
        let timeout :: m ()
            timeout = do
                liftIO . threadDelay $ 30000000
                pure ()
        choice <- lift $ race timeout worker
        case choice of
          Left killed -> do
              logWarning $ sformat ("sendToNode : timed out after 30 seconds to destination " % shown) addr
          Right ok -> pure ()
      where
        -- [CSL-4][TW-47]: temporary code, to refactor to subscriptions (after TW-47)
        listenOutboundDo = KademliaDHT (asks kdcListenByBinding) >>= ($ AtConnTo addr)
        listenOutbound = listenOutboundDo `catches` [Handler handleAL, Handler handleTE]
        handleAL (AlreadyListeningOutbound _) = do
            logWarning $ sformat ("Already listening on outbound connection to " % shown) addr
            return $ pure ()
        handleTE e@(SomeException _) = do
            logWarning $ sformat ("Error listening on outbound connection to " %
                                shown % ": " % build) addr e
            return $ pure ()
        updateClosers closer = do
            tvar <- KademliaDHT (asks kdcAuxClosers)
            closers <- atomically $ do
                closers <- readTVar tvar
                writeTVar tvar (closer : closers)
                pure closers
            logInfo $ sformat ("updateClosers : total number of closers is now " % int) (length closers)

rejoinNetwork :: (MonadIO m, WithLogger m, MonadCatch m) => KademliaDHT m ()
rejoinNetwork = withDhtLogger $ do
    peers <- getKnownPeers
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
    when (null peers) $ do
      logWarning "Empty known peer list"
      init <- KademliaDHT $ asks (kdiInitialPeers . kdcDHTInstance_)
      joinNetworkNoThrow init

instance (MonadIO m, MonadCatch m, WithLogger m) => MonadDHT (KademliaDHT m) where

  joinNetwork [] = throwM AllPeersUnavailable
  joinNetwork nodes = do
      inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
      loggerName <- getLoggerName
      asyncs <- mapM
          (liftIO . usingLoggerName loggerName . async . joinNetwork' inst)
          nodes
      waitAnyUnexceptional asyncs >>= handleRes
    where
      handleRes (Just _) = pure ()
      handleRes _        = throwM AllPeersUnavailable

  discoverPeers type_ = do
    inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filterByNodeType type_ <$> getKnownPeers

  getKnownPeers = do
      myId <- currentNodeKey
      (inst, initialPeers, explicitInitial) <- KademliaDHT $ (,,)
          <$> asks (kdiHandle . kdcDHTInstance_)
          <*> asks (kdiInitialPeers . kdcDHTInstance_)
          <*> asks (kdiExplicitInitial . kdcDHTInstance_)
      extendPeers myId (if explicitInitial then initialPeers else []) <$> liftIO (K.dumpPeers inst)
    where
      extendPeers myId initial
        = map snd
        . HM.toList
        . HM.delete myId
        . flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial
        . HM.fromList
        . map (\(toDHTNode -> n) -> (dhtNodeId n, n))


  currentNodeKey = KademliaDHT $ asks (kdiKey . kdcDHTInstance_)

  dhtLoggerName _ = "kademlia"

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

joinNetwork'
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
    let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
    res <- liftIO $ K.joinNetwork inst node'
    case res of
        K.JoinSucces -> pure ()
        K.NodeDown -> throwM NodeDown
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: node " % build % " already contains us") node

-- [TW-84]: move to serokell-core or time-warp?
waitAnyUnexceptional
    :: (MonadIO m, WithLogger m)
    => [Async a] -> m (Maybe (Async a, a))
waitAnyUnexceptional asyncs = liftIO (waitAnyCatch asyncs) >>= handleRes
  where
    handleRes (async', Right res) = pure $ Just (async', res)
    handleRes (async', Left e) = do
      logWarning $ sformat ("waitAnyUnexceptional: caught error " % shown) e
      if null asyncs'
         then pure Nothing
         else waitAnyUnexceptional asyncs'
      where asyncs' = filter (/= async') asyncs
