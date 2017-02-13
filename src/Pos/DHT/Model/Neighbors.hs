{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Model.Neighbors
  ( sendToNeighbors
  , sendToNode
  , converseToNeighbors
  , converseToNode

  , sendToNeighbors'
  , sendToNode'
  , converseToNeighbors'
  , converseToNode'

  , setPreferredInterface
  , getPreferredInterface
  ) where


import           Formatting          (int, sformat, shown, (%))
import           Mockable            (MonadMockable, forConcurrently, handleAll, throw)
import           Node                (ConversationActions, NodeId, SendActions (..))
import           Node.Message        (Message, Packable, Serializable, Unpackable)
import qualified STMContainers.Map   as STMMap
import           System.Wlog         (WithLogger, logWarning)
import           Universum           hiding (catchAll, forConcurrently)

import           Pos.Constants       (isDevelopment, neighborsSendThreshold)
import           Pos.Context         (WithNodeContext (..), ncPreferredInterfaces)
import           Pos.DHT.Model.Class (MonadDHT (..))
import           Pos.DHT.Model.Types (DHTNode (..), DHTNodeType (..), addressToNodeId',
                                      filterByNodeType)
import           Pos.Util.TimeWarp   (NetworkAddress)

----------------------------------------------------------------------------
-- Peer discovery
----------------------------------------------------------------------------

getNodesWithCheck :: (MonadDHT m, WithLogger m) => m [DHTNode]
getNodesWithCheck = do
    nodes <- do
        nodes_ <- filterByNodeType DHTFull <$> getKnownPeers
        if length nodes_ < neighborsSendThreshold
           then discoverPeers DHTFull
           else return nodes_
    when (length nodes < neighborsSendThreshold) $
        logWarning $ sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            (length nodes) (neighborsSendThreshold :: Int)
    return nodes

----------------------------------------------------------------------------
-- Sending functions
----------------------------------------------------------------------------

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: (WithNodeContext ssc m
       ,MonadIO m
       ,MonadDHT m
       ,MonadMockable m
       ,Serializable packing body
       ,WithLogger m
       ,Message body)
    => SendActions packing m -> body -> m ()
sendToNeighbors = sendToNeighbors' $
    Just (setPreferredInterface, getPreferredInterface)

sendToNode
    :: (WithNodeContext ssc m
       ,MonadIO m
       ,MonadMockable m
       ,Serializable packing body
       ,Message body)
    => SendActions packing m
    -> NetworkAddress
    -> body
    -> m ()
sendToNode = sendToNode' $
    Just (setPreferredInterface, getPreferredInterface)

converseToNeighbors
    :: (WithNodeContext ssc m
       ,MonadIO m
       ,MonadDHT m
       ,MonadMockable m
       ,WithLogger m
       ,Unpackable packing rcv
       ,Packable packing snd
       ,Message snd)
    => SendActions packing m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors = converseToNeighbors' $
    Just (setPreferredInterface, getPreferredInterface)

converseToNode
    :: (WithNodeContext ssc m
       ,MonadIO m
       ,MonadMockable m
       ,Unpackable packing rcv
       ,Packable packing snd
       ,Message snd)
    => SendActions packing m
    -> NetworkAddress
    -> (NodeId -> ConversationActions snd rcv m -> m t)
    -> m t
converseToNode = converseToNode' $
    Just (setPreferredInterface, getPreferredInterface)

----------------------------------------------------------------------------
-- Working with preferred interfaces
----------------------------------------------------------------------------

setPreferredInterface
    :: (WithNodeContext ssc m, MonadIO m)
    => NetworkAddress -> Word32 -> m ()
setPreferredInterface addr i = do
    mp <- ncPreferredInterfaces <$> getNodeContext
    liftIO $ atomically $ STMMap.insert i addr mp

getPreferredInterface
    :: (WithNodeContext ssc m, MonadIO m)
    => NetworkAddress -> m Word32
getPreferredInterface addr = do
    mp <- ncPreferredInterfaces <$> getNodeContext
    liftIO $ atomically $ fromMaybe 0 <$> STMMap.lookup addr mp

----------------------------------------------------------------------------
-- Low-level sending functions (that don't necessarily require IO)
----------------------------------------------------------------------------

sendToNeighbors'
    :: (MonadDHT m
       ,MonadMockable m
       ,Serializable packing body
       ,WithLogger m
       ,Message body)
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions packing m -> body -> m ()
sendToNeighbors' mbPrefs sendActions msg = do
    nodes <- getNodesWithCheck
    void $ forConcurrently nodes $ \node ->
        handleAll (logSendErr node) $
        sendToNode' mbPrefs sendActions (dhtAddr node) msg
  where
    logSendErr node e = logWarning $ sformat ("Error sending to "%shown%": "%shown) node e

converseToNeighbors'
    :: (MonadDHT m
       ,MonadMockable m
       ,WithLogger m
       ,Unpackable packing rcv
       ,Packable packing snd
       ,Message snd)
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions packing m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors' mbPrefs sendActions convHandler = do
    nodes <- getNodesWithCheck
    void $ forConcurrently nodes $ \node ->
        handleAll (logErr node) $
        converseToNode' mbPrefs sendActions (dhtAddr node) convHandler
  where
    logErr node e = logWarning $ sformat ("Error in conversation to "%shown%": "%shown) node e

sendToNode'
    :: (MonadMockable m
       ,Serializable packing body
       ,Message body)
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions packing m
    -> NetworkAddress
    -> body
    -> m ()
sendToNode' mbPrefs sendActions addr msg = do
    pref <- case mbPrefs of
        Nothing                -> return 0
        Just (_, getInterface) -> getInterface addr
    let sendToOther = do
            whenJust mbPrefs $ \(setInterface, _) ->
                setInterface addr (1 - pref)
            sendTo sendActions (addressToNodeId' (1 - pref) addr) msg
    let handleE e = if isDevelopment then sendToOther else throw e
    handleAll handleE $
        sendTo sendActions (addressToNodeId' pref addr) msg

converseToNode'
    :: (MonadMockable m
       ,Unpackable packing rcv
       ,Packable packing snd
       ,Message snd)
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions packing m
    -> NetworkAddress
    -> (NodeId -> ConversationActions snd rcv m -> m t)
    -> m t
converseToNode' mbPrefs sendActions addr handler = do
    pref <- case mbPrefs of
        Nothing                -> return 0
        Just (_, getInterface) -> getInterface addr
    let tryConnect i =
            let peerId = addressToNodeId' i addr
            in  withConnectionTo sendActions peerId $ handler peerId
    let sendToOther = do
            x <- tryConnect (1 - pref)
            -- TODO: actually wrong because tryConnect could've thrown an
            -- exception again, I guess
            whenJust mbPrefs $ \(setInterface, _) ->
                setInterface addr (1 - pref)
            return x
    let handleE e = if isDevelopment then sendToOther else throw e
    handleAll handleE $ tryConnect pref
