{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TossT monad transformer. Single-threaded.

module Pos.Ssc.GodTossing.Toss.Trans
       ( TossT (..)
       , runTossT
       , evalTossT
       , execTossT
       ) where

import           Control.Lens                  (at, iso, (%=), (.=))
import           Control.Monad.Base            (MonadBase (..))
import           Control.Monad.Except          (MonadError)
import           Control.Monad.Trans.Class     (MonadTrans)
import           Control.Monad.Trans.Control   (ComposeSt, MonadBaseControl (..),
                                                MonadTransControl (..), StM,
                                                defaultLiftBaseWith, defaultLiftWith,
                                                defaultRestoreM, defaultRestoreT)
import qualified Data.HashMap.Strict           as HM
import           Mockable                      (ChannelT, Promise, SharedAtomicT,
                                                ThreadId)
import           Serokell.Util.Lens            (WrappedM (..))
import           System.Wlog                   (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                   (WithNodeContext)
import           Pos.DB.Class                  (MonadDB)
import           Pos.Slotting                  (MonadSlots (..), MonadSlotsData)
import           Pos.Ssc.Extra                 (MonadSscMem)
import           Pos.Ssc.GodTossing.Core       (deleteSignedCommitment, getCertId,
                                                insertSignedCommitment)
import           Pos.Ssc.GodTossing.Toss.Class (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Types (TossModifier (..), tmCertificates,
                                                tmCommitments, tmOpenings, tmShares)
import           Pos.Util.JsonLog              (MonadJL (..))

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TossModifier and implements
-- writable MonadToss.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
newtype TossT m a = TossT
    { getTossT :: StateT TossModifier m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadThrow
               , MonadSlotsData
               , MonadSlots
               , MonadCatch
               , MonadIO
               , HasLoggerName
               , MonadTrans
               , MonadError e
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadDB
               , MonadMask
               , MonadSscMem mem
               , MonadBase io
               )

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTossT :: TossModifier -> TossT m a -> m (a, TossModifier)
runTossT m (TossT s) = runStateT s m

evalTossT :: Functor m => TossModifier -> TossT m a -> m a
evalTossT m = fmap fst . runTossT m

execTossT :: Functor m => TossModifier -> TossT m a -> m TossModifier
execTossT m = fmap snd . runTossT m

----------------------------------------------------------------------------
-- MonadToss
----------------------------------------------------------------------------

instance MonadTossRead m =>
         MonadTossRead (TossT m) where
    getCommitments = TossT $ (<>) <$> use tmCommitments <*> getCommitments
    getOpenings = TossT $ (<>) <$> use tmOpenings <*> getOpenings
    getShares = TossT $ (<>) <$> use tmShares <*> getShares
    getVssCertificates =
        TossT $ (<>) <$> use tmCertificates <*> lift getVssCertificates
    getStableCertificates = TossT . getStableCertificates
    getRichmen = TossT . getRichmen

instance MonadToss m =>
         MonadToss (TossT m) where
    putCommitment signedComm =
        TossT $ tmCommitments %= insertSignedCommitment signedComm
    putOpening id op =
        TossT $ tmOpenings . at id .= Just op
    putShares id sh =
        TossT $ tmShares . at id .= Just sh
    putCertificate cert =
        TossT $ tmCertificates %= HM.insert (getCertId cert) cert
    delCommitment id =
        TossT $ tmCommitments %= deleteSignedCommitment id
    delOpening id =
        TossT $ tmOpenings . at id .= Nothing
    delShares id =
        TossT $ tmShares . at id .= Nothing
    resetCO = TossT $ do
        tmCommitments .= mempty
        tmOpenings .= mempty
        tmCertificates .= mempty
        resetCO
    resetShares = TossT $ do
        tmShares .= mempty
        resetShares
    setEpochOrSlot = TossT . setEpochOrSlot

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (TossT m) = ThreadId m
type instance Promise (TossT m) = Promise m
type instance SharedAtomicT (TossT m) = SharedAtomicT m
type instance ChannelT (TossT m) = ChannelT m

instance Monad m => WrappedM (TossT m) where
    type UnwrappedM (TossT m) = StateT TossModifier m
    _WrappedM = iso getTossT TossT

instance MonadTransControl TossT where
    type StT (TossT) a = StT (StateT TossModifier) a
    liftWith = defaultLiftWith TossT getTossT
    restoreT = defaultRestoreT TossT

instance MonadBaseControl IO m => MonadBaseControl IO (TossT m) where
    type StM (TossT m) a = ComposeSt TossT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM
