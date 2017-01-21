-- | Functions for operating with messages of update system

module Pos.Wallet.Update
       ( submitVote
       , submitUpdateProposal
       ) where

import           Mockable                  (forConcurrently)
import           Node                      (SendActions)
import           Pos.Util.TimeWarp         (NetworkAddress)
import           Universum

import           Pos.Binary                ()
import           Pos.Communication.BiP     (BiP)
import           Pos.Communication.Methods (sendUpdateProposal, sendVote)
import           Pos.Crypto                (SecretKey, hash, sign, toPublic)
import           Pos.DHT.Model             (dhtAddr, getKnownPeers)
import           Pos.Update                (UpdateProposal, UpdateVote (..))
import           Pos.WorkMode              (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: MinWorkMode m
    => SendActions BiP m
    -> UpdateVote
    -> m ()
submitVote sendActions voteUpd = do
    na <- fmap dhtAddr <$> getKnownPeers
    void $ forConcurrently na $
        \addr -> sendVote sendActions addr voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: MinWorkMode m
    => SendActions BiP m
    -> SecretKey
    -> UpdateProposal
    -> m ()
submitUpdateProposal sendActions sk prop = do
    let upid = hash prop
    let initUpdVote = UpdateVote
            { uvKey        = toPublic sk
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = sign sk (upid, True)
            }
    na <- fmap dhtAddr <$> getKnownPeers
    void $ forConcurrently na $
        \addr -> sendUpdateProposal sendActions addr upid prop [initUpdVote]
