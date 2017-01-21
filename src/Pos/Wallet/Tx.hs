-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , makeMOfNTx
       , submitTx
       , submitTxRaw
       , sendTxId
       , createTx
       , createMOfNTx
       ) where

import           Control.Monad.Except  (ExceptT (..), runExceptT)
import           Formatting            (build, sformat, (%))
import           Node                  (SendActions)
import           System.Wlog           (logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication.BiP (BiP)
import           Pos.Crypto            (SecretKey, hash, toPublic)
import           Pos.DHT.Model         (DHTNode, getKnownPeers, sendToNodes)
import           Pos.Txp.Types         (TxMsgTag (..))
import           Pos.Types             (TxAux, TxId, TxOutAux, makePubKeyAddress, txaF)
import           Pos.Util.Relay        (InvMsg (..))
import           Pos.WorkMode          (MinWorkMode)

import           Pos.Wallet.Tx.Pure    (TxError, createMOfNTx, createTx, makeMOfNTx,
                                        makePubKeyTx)
import           Pos.Wallet.WalletMode (TxMode, getOwnUtxo, saveTx)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => SendActions BiP m
    -> SecretKey
    -> [TxOutAux]
    -> m (Either TxError TxAux)
submitTx sendActions sk outputs = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ toPublic sk
    nodes <- getKnownPeers
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo sk outputs
        lift $ submitTxRaw sendActions nodes txw
        return txw

-- | Save tx locally and notify neighbors about it
submitTxRaw
    :: TxMode ssc m
    => SendActions BiP m
    -> [DHTNode]
    -> TxAux
    -> m ()
submitTxRaw sendActions nodes txw = do
    let txId = hash (txw ^. _1)
    logInfo $ sformat ("Saving transaction: "%txaF) txw
    saveTx (txId, txw)
    sendTxId sendActions nodes txId

-- | Notify neighbors about transactions
sendTxId :: MinWorkMode m => SendActions BiP m -> [DHTNode] -> TxId -> m ()
sendTxId sendActions nodes txId = do
    let msg = InvMsg {imTag = TxMsgTag, imKeys = one txId}
    logInfo $ sformat ("Submitting transaction id: "%build) txId
    sendToNodes sendActions msg nodes
