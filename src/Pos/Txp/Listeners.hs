{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       , txStubListeners
       , processTx
       ) where

import qualified Data.HashMap.Strict         as HM
import           Formatting                  (build, sformat, stext, (%))
import           Node                        (ListenerAction (..))
import           Serokell.Util.Verify        (VerificationRes (..))
import           System.Wlog                 (WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Communication.BiP       (BiP (..))
import           Pos.Crypto                  (hash)
import           Pos.Slotting                (getCurrentSlot)
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Class               (getMemPool)
import           Pos.Txp.Logic               (processTx)
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Txp.Types.Types         (MemPool (..), ProcessTxRes (..))
import           Pos.Types                   (TxAux, TxId)
import           Pos.Util                    (stubListenerOneMsg)
import           Pos.Util.Relay              (DataMsg, InvMsg, MempoolMsg, Relay (..),
                                              ReqMsg, handleDataL, handleInvL,
                                              handleMempoolL, handleReqL)
import           Pos.WorkMode                (WorkMode)

txListeners
    :: WorkMode ssc m
    => [ListenerAction BiP m]
txListeners =
    [ handleInvTx
    , handleReqTx
    , handleDataTx
    , handleMempoolTx
    ]

handleInvTx
    :: WorkMode ssc m
    => ListenerAction BiP m
handleInvTx = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg TxId TxMsgTag) ->
    handleInvL i peerId sendActions

handleReqTx
    :: WorkMode ssc m
    => ListenerAction BiP m
handleReqTx = ListenerActionOneMsg $ \peerId sendActions (r :: ReqMsg TxId TxMsgTag) ->
    handleReqL r peerId sendActions

handleDataTx
    :: WorkMode ssc m
    => ListenerAction BiP m
handleDataTx = ListenerActionOneMsg $ \peerId sendActions (d :: DataMsg TxId TxMsgContents) ->
    handleDataL d peerId sendActions

handleMempoolTx
    :: WorkMode ssc m
    => ListenerAction BiP m
handleMempoolTx = ListenerActionOneMsg $ \peerId sendActions (m :: MempoolMsg TxMsgTag) ->
    handleMempoolL m peerId sendActions

txStubListeners
    :: WithLogger m
    => Proxy ssc -> [ListenerAction BiP m]
txStubListeners p =
    [ stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (InvMsg TxId TxMsgTag)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (ReqMsg TxId TxMsgTag)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MempoolMsg TxMsgTag)) p
    , stubListenerOneMsg $
        (const Proxy :: Proxy ssc -> Proxy (DataMsg TxId TxMsgContents)) p
    ]

instance ( WorkMode ssc m
         ) => Relay m TxMsgTag TxId TxMsgContents where
    contentsToTag _ = pure TxMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyMempoolTag _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ txId = not . HM.member txId  . localTxs <$> getMemPool

    handleReq _ txId = fmap toContents . HM.lookup txId . localTxs <$> getMemPool
      where
        toContents (_, (tx, tw, td)) = TxMsgContents tx tw td

    handleMempool _ = HM.keys . localTxs <$> getMemPool

    handleData (TxMsgContents tx tw td) _ = handleTxDo (hash tx, (tx, tw, td))

-- Real tx processing
-- CHECK: @handleTxDo
-- #processTx
handleTxDo
    :: WorkMode ssc m
    => (TxId, TxAux) -> m Bool
handleTxDo tx = do
    sid <- getCurrentSlot
    res <- processTx (fst tx, (sid, snd tx))
    let txId = fst tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%build%" failed to verify: "%stext) txId msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) txId
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) txId
    return (res == PTRadded)
