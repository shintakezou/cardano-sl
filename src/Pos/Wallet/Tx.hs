-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , makeMOfNTx
       , submitTx
       , submitTxRaw
       , submitTx'
       , submitTxRaw'
       , createTx
       , createMOfNTx
       ) where

import           Control.Lens              ((^.), _1)
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Formatting                (build, sformat, (%))
import           Mockable                  (mapConcurrently)
import           Node                      (SendActions)
import           Pos.Util.TimeWarp         (NetworkAddress)
import           System.Wlog               (logError, logInfo)
import           Universum                 hiding (mapConcurrently)

import           Pos.Binary                ()
import           Pos.Communication.BiP     (BiP)
import           Pos.Communication.Methods (sendTx, sendTx')
import           Pos.Context               (WithNodeContext)
import           Pos.Crypto                (SecretKey, hash, toPublic)
import           Pos.DHT.Model.Neighbors   (getPreferredInterface, setPreferredInterface)
import           Pos.Types                 (SlotId, TxAux, TxOutAux, makePubKeyAddress,
                                            txaF)
import           Pos.WorkMode              (MinWorkMode)

import           Pos.Wallet.Tx.Pure        (TxError, createMOfNTx, createTx, makeMOfNTx,
                                            makePubKeyTx)
import           Pos.Wallet.WalletMode     (TxMode, getOwnUtxo, saveTx)
import           Pos.WorkMode              (MinWorkMode)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: (TxMode ssc m, WithNodeContext ssc m)
    => SendActions BiP m
    -> SecretKey
    -> [NetworkAddress]
    -> [TxOutAux]
    -> SlotId
    -> m (Either TxError TxAux)
submitTx = submitTx' $
    Just (setPreferredInterface, getPreferredInterface)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, WithNodeContext ssc m)
    => SendActions BiP m -> [NetworkAddress] -> TxAux -> m ()
submitTxRaw = submitTxRaw' $
    Just (setPreferredInterface, getPreferredInterface)

submitTx'
    :: TxMode ssc m
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions BiP m
    -> SecretKey
    -> [NetworkAddress]
    -> [TxOutAux]
    -> SlotId
    -> m (Either TxError TxAux)
submitTx' _ _ _ _ [] _ = do
    logError "No addresses to send"
    return (Left "submitTx failed")
submitTx' mbPrefs sendActions sk na outputs sid = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ toPublic sk
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo sk outputs
        let txId = hash (txw ^. _1)
        lift $ submitTxRaw' mbPrefs sendActions na txw
        lift $ saveTx (txId, (sid, txw))
        return txw

-- | Send the ready-to-use transaction
submitTxRaw'
    :: MinWorkMode m
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions BiP m -> [NetworkAddress] -> TxAux -> m ()
submitTxRaw' mbPrefs sa na tx = do
    let txId = hash (tx ^. _1)
    logInfo $ sformat ("Submitting transaction: "%txaF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    void $ mapConcurrently (flip (sendTx' mbPrefs sa) tx) na
