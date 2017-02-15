{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Listeners
       ( usListeners
       ) where

import           Node                     (ListenerAction (..), recv)
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Relay         ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Update.Types         (ProposalMsgTag (..), UpId, UpdateProposal (..))
import           Pos.Util.Relay           (DataMsg, InvMsg, MempoolMsg, Relay (..),
                                           ReqMsg, handleDataL, handleInvL,
                                           handleMempoolL, handleReqL)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to update system
usListeners
    :: (WorkMode ssc m)
    => [ListenerAction BiP m]
usListeners =
    [ handleInvProposal
    , handleReqProposal
    , handleMempoolProposal
    , handleDataProposal
    ]

handleInvProposal :: WorkMode ssc m => ListenerAction BiP m
handleInvProposal = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg UpId ProposalMsgTag) ->
    handleInvL i peerId sendActions

handleReqProposal :: WorkMode ssc m => ListenerAction BiP m
handleReqProposal = ListenerActionOneMsg $ \peerId sendActions (i :: ReqMsg UpId ProposalMsgTag) ->
    handleReqL i peerId sendActions

handleMempoolProposal :: WorkMode ssc m => ListenerAction BiP m
handleMempoolProposal = ListenerActionConversation $ \peerId conv -> do
    mm :: Maybe (MempoolMsg ProposalMsgTag) <- recv conv
    whenJust mm $ \m -> handleMempoolL m peerId conv

handleDataProposal :: WorkMode ssc m => ListenerAction BiP m
handleDataProposal = ListenerActionOneMsg $ \peerId sendActions (i :: DataMsg UpId UpdateProposal) ->
    handleDataL i peerId sendActions

instance WorkMode ssc m =>
         Relay m ProposalMsgTag UpId UpdateProposal where
    contentsToTag _ = pure ProposalMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyMempoolTag _ = pure VerSuccess
    -- TODO: maybe somehow check that versions are not decreasing or whatevs?
    verifyDataContents UpdateProposal{..} = pure VerSuccess

    handleInv _ _ = notImplemented
    handleReq _ _ = notImplemented
    handleData _ _ = notImplemented
    handleMempool _ = notImplemented
