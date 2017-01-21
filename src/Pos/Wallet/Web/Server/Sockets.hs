{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module for websockets implementation of Daedalus API

module Pos.Wallet.Web.Server.Sockets
       ( WalletWebSockets
       , WebWalletSockets
       , MonadWalletWebSockets (..)
       , ConnectionsVar
       , initWSConnection
       , closeWSConnection
       , upgradeApplicationWS
       , notify
       , runWalletWS
       , getWalletWebSocketsState
       ) where

import qualified Network.WebSockets             as WS
import           Pos.Wallet.Web.ClientTypes     (NotifyEvent (ConnectionClosed, ConnectionOpened))

import           Control.Concurrent.STM.TVar    (TVar, newTVarIO, readTVarIO, writeTVar)
import           Control.Lens                   (iso)
import           Control.Monad.Trans            (MonadTrans (..))
import           Data.Aeson                     (encode)
import           Mockable                       (ChannelT, MFunctor',
                                                 Mockable (liftMockable), Promise,
                                                 SharedAtomicT, ThreadId,
                                                 liftMockableWrappedM)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Pos.Aeson.ClientTypes          ()
import           Pos.Context                    (WithNodeContext)
import qualified Pos.DB                         as Modern
import           Pos.Delegation.Class           (MonadDelegation)
import           Pos.DHT.Model                  (MonadDHT)
import           Pos.Slotting                   (MonadSlots)
import           Pos.Txp.Class                  (MonadTxpLD)
import qualified Pos.Update                     as US
import           Pos.Wallet.Context             (WithWalletContext)
import           Pos.Wallet.KeyStorage          (MonadKeys)
import           Pos.Wallet.State               (MonadWalletDB)
import           Pos.Wallet.WalletMode          (MonadBalances, MonadBlockchainInfo,
                                                 MonadTxHistory)
import           Pos.Wallet.Web.State           (MonadWalletWebDB)
import           Serokell.Util.Lens             (WrappedM (..))
import           System.Wlog                    (CanLog, HasLoggerName)
import           Universum

-- NODE: for now we are assuming only one client will be used. If there will be need for multiple clients we should extend and hold multiple connections here.
-- We might add multiple clients when we add user profiles but I am not sure if we are planning on supporting more at all.
type ConnectionsVar = TVar (Maybe WS.Connection)

initWSConnection :: MonadIO m => m ConnectionsVar
initWSConnection = liftIO $ newTVarIO Nothing

closeWSConnection :: MonadIO m => ConnectionsVar -> m ()
closeWSConnection var = do
    conn <- liftIO $ readTVarIO var
    atomically $ writeTVar var Nothing
    liftIO $ maybe mempty (flip WS.sendClose ConnectionClosed) conn

switchConnection :: ConnectionsVar -> WS.ServerApp
switchConnection var pending = do
    conn <- WS.acceptRequest pending
    closeWSConnection var
    atomically . writeTVar var $ Just conn
    sendWS var ConnectionOpened
    WS.forkPingThread conn 30
    forever (ignoreData conn) `finally` releaseResources
  where
    ignoreData :: WS.Connection -> IO Text
    ignoreData = WS.receiveData
    releaseResources = closeWSConnection var -- TODO: log

-- If there is a new pending ws connection, the old connection will be replaced with new one.
-- FIXME: this is not safe because someone can kick out previous ws connection. Authentication can solve this issue. Solution: reject pending connection if ws handshake doesn't have valid auth session token
upgradeApplicationWS :: ConnectionsVar -> Application -> Application
upgradeApplicationWS wsConn = websocketsOr WS.defaultConnectionOptions $ switchConnection wsConn

-- sendClose :: MonadIO m => ConnectionsVar -> NotifyEvent -> m ()
-- sendClose = send WS.sendClose

-- Sends notification msg to connected client. If there is no connection, notification msg will be ignored.
sendWS :: MonadIO m => ConnectionsVar -> NotifyEvent -> m ()
sendWS = send WS.sendTextData

send :: MonadIO m => (WS.Connection -> NotifyEvent -> IO ()) -> ConnectionsVar -> NotifyEvent -> m ()
send f connVar msg = liftIO $ maybe mempty (flip f msg) =<< readTVarIO connVar

instance WS.WebSocketsData NotifyEvent where
    fromLazyByteString _ = panic "Attempt to deserialize NotifyEvent is illegal"
    toLazyByteString = encode

--------
-- API
--------

-- | Holder for web wallet data
newtype WalletWebSockets m a = WalletWebSockets
    { getWalletWS :: ReaderT ConnectionsVar m a
    } deriving (Functor, Applicative, Monad, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                MonadWalletDB, WithWalletContext,
                MonadDHT, MonadSlots,
                CanLog, MonadKeys, MonadBalances,
                MonadTxHistory, MonadBlockchainInfo, WithNodeContext ssc,
                Modern.MonadDB ssc, MonadTxpLD ssc, MonadWalletWebDB, MonadDelegation, US.MonadUSMem)

instance Monad m => WrappedM (WalletWebSockets m) where
    type UnwrappedM (WalletWebSockets m) = ReaderT ConnectionsVar m
    _WrappedM = iso getWalletWS WalletWebSockets

instance MonadTrans WalletWebSockets where
    lift = WalletWebSockets . lift

-- | MonadWalletWebSockets stands for monad which is able to get web wallet sockets
class Monad m => MonadWalletWebSockets m where
    getWalletWebSockets :: m ConnectionsVar

instance Monad m => MonadWalletWebSockets (WalletWebSockets m) where
    getWalletWebSockets = WalletWebSockets ask

type instance ThreadId (WalletWebSockets m) = ThreadId m
type instance Promise (WalletWebSockets m) = Promise m
type instance SharedAtomicT (WalletWebSockets m) = SharedAtomicT m
type instance ChannelT (WalletWebSockets m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (WalletWebSockets m) (ReaderT ConnectionsVar m)
         , MFunctor' d (ReaderT ConnectionsVar m) m
         ) => Mockable d (WalletWebSockets m) where
    liftMockable = liftMockableWrappedM

runWalletWS :: ConnectionsVar -> WalletWebSockets m a -> m a
runWalletWS conn = flip runReaderT conn . getWalletWS

type WebWalletSockets m = (MonadWalletWebSockets m, MonadIO m)

notify :: WebWalletSockets m => NotifyEvent -> m ()
notify msg = getWalletWebSockets >>= flip sendWS msg

getWalletWebSocketsState :: Monad m => WalletWebSockets m ConnectionsVar
getWalletWebSocketsState = WalletWebSockets ask
