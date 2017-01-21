{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all basic types for @cardano-sl@ update system.

module Pos.Update.Core.Types
       (
         -- * UpdateProposal and related
         UpdateProposal (..)
       , UpId
       , UpAttributes
       , UpdateData (..)
       , SystemTag (getSystemTag)
       , mkSystemTag
       , systemTagMaxLength
       , patakUpdateData

         -- * UpdateVote and related
       , UpdateVote (..)
       , VoteId
       , StakeholderVotes
       , UpdateProposals
       , LocalVotes
       , mkVoteId

         -- * Payload and proof
       , UpdatePayload (..)
       , UpdateProof
       , mkUpdateProof

       -- * VoteState
       , VoteState (..)
       , canCombineVotes
       , combineVotes
       , isPositiveVote
       , newVoteState
       ) where

import           Data.Char                  (isAscii)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, int, (%))
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Util.Text         (listJson)
import           Universum                  hiding (show)

import           Pos.Binary.Class           (Bi)
import           Pos.Crypto                 (Hash, PublicKey, Signature, hash, unsafeHash)
import           Pos.Data.Attributes        (Attributes)
import           Pos.Script.Type            (ScriptVersion)
import           Pos.Types.Version          (ProtocolVersion, SoftwareVersion)
import           Pos.Util                   (Raw)

----------------------------------------------------------------------------
-- UpdateProposal and related
----------------------------------------------------------------------------

-- | Tag of system for which update data is purposed, e.g. win64, mac32
newtype SystemTag = SystemTag { getSystemTag :: Text }
  deriving (Eq, Ord, Show, Generic, Buildable, Hashable, Lift, Typeable)

systemTagMaxLength :: Integral i => i
systemTagMaxLength = 10

mkSystemTag :: MonadFail m => Text -> m SystemTag
mkSystemTag tag | T.length tag > systemTagMaxLength
                    = fail "SystemTag: too long string passed"
                | T.any (not . isAscii) tag
                    = fail "SystemTag: not ascii string passed"
                | otherwise
                    = pure $ SystemTag tag

-- | ID of softwaree update proposal
type UpId = Hash UpdateProposal

type UpAttributes = Attributes ()

-- | Proposal for software update
data UpdateProposal = UpdateProposal
    { upProtocolVersion :: !ProtocolVersion
    , upScriptVersion   :: !ScriptVersion
    , upSoftwareVersion :: !SoftwareVersion
    , upData            :: !(HM.HashMap SystemTag UpdateData)
    -- ^ UpdateData for each system which this update affects.
    -- It must be non-empty.
    , upAttributes      :: !UpAttributes
    -- ^ Attributes which are currently empty, but provide
    -- extensibility.
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable UpdateProposal where
    build UpdateProposal {..} =
      bprint (build%
              " { protocol v"%build%
              ", scripts v"%build%
              ", tags: "%listJson%
              ", no attributes "%
              " }")
        upSoftwareVersion upProtocolVersion upScriptVersion (HM.keys upData)

instance Buildable (UpdateProposal, [UpdateVote]) where
    build (up, votes) =
      bprint (build%" with votes: "%listJson)
             up votes

-- | Data which describes update. It is specific for each system.
data UpdateData = UpdateData
    { udAppDiffHash  :: !(Hash Raw)
    -- ^ Hash of binary diff between two applications. This diff can
    -- be passed to updater to create new application.
    , udPkgHash      :: !(Hash Raw)
    -- ^ Hash of package to install new application. This package can
    -- be used to install new application from scratch instead of
    -- updating existing application.
    , udUpdaterHash  :: !(Hash Raw)
    -- ^ Hash if update application which can be used to install this
    -- update (relevant only when updater is used, not package).
    , udMetadataHash :: !(Hash Raw)
    -- ^ Hash of metadata relevant to this update.  It is raw hash,
    -- because metadata can include image or something
    -- (maybe). Anyway, we can always use `unsafeHash`.
    } deriving (Eq, Show, Generic, Typeable)

patakUpdateData :: HM.HashMap SystemTag UpdateData
patakUpdateData =
    let b = "bardaq"
        h = unsafeHash b
    in  HM.fromList [(SystemTag b, UpdateData h h h h)]

----------------------------------------------------------------------------
-- UpdateVote and related
----------------------------------------------------------------------------

type VoteId = (UpId, PublicKey, Bool)

-- | Vote for update proposal
data UpdateVote = UpdateVote
    { -- | Public key of stakeholder, who votes
      uvKey        :: !PublicKey
    , -- | Proposal to which this vote applies
      uvProposalId :: !UpId
    , -- | Approval/rejection bit
      uvDecision   :: !Bool
    , -- | Signature of (Update proposal, Approval/rejection bit)
      --   by stakeholder
      uvSignature  :: !(Signature (UpId, Bool))
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable UpdateVote where
    build UpdateVote {..} =
      bprint ("Update Vote { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             uvKey uvProposalId uvDecision

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             pk upId dec

mkVoteId :: UpdateVote -> VoteId
mkVoteId UpdateVote{..} = (uvProposalId, uvKey, uvDecision)

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Update System payload. 'Pos.Types.BodyProof' contains 'UpdateProof' = @Hash UpdatePayload@.
data UpdatePayload = UpdatePayload
    { upProposal :: !(Maybe UpdateProposal)
    , upVotes    :: ![UpdateVote]
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable UpdatePayload where
    build UpdatePayload{..} =
      bprint (build%", "%int%" votes")
             (maybe "no proposal" Buildable.build upProposal)
             (length upVotes)

instance Default UpdatePayload where
    def = UpdatePayload Nothing []

-- | Proof that body of update message contains 'UpdatePayload'.
type UpdateProof = Hash UpdatePayload

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash

----------------------------------------------------------------------------
-- VoteState
----------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic)

-- | Create new VoteState from bool, which is simple vote, not revote.
newVoteState :: Bool -> VoteState
newVoteState True  = PositiveVote
newVoteState False = NegativeVote

isPositiveVote :: VoteState -> Bool
isPositiveVote PositiveVote   = True
isPositiveVote PositiveRevote = True
isPositiveVote _              = False

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function returns
-- 'Nothing' if decision can't be applied. 'canCombineVotes' can be
-- used to check whether it will be successful.
combineVotes :: Bool -> Maybe VoteState -> Maybe VoteState
combineVotes decision oldVote =
    case (decision, oldVote) of
        (True, Nothing)            -> Just PositiveVote
        (False, Nothing)           -> Just NegativeVote
        (True, Just NegativeVote)  -> Just PositiveRevote
        (False, Just PositiveVote) -> Just NegativeRevote
        (_, Just _)                -> Nothing

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = HashMap PublicKey VoteState

type UpdateProposals = HashMap UpId UpdateProposal
type LocalVotes = HashMap UpId (HashMap PublicKey UpdateVote)

----------------------------------------------------------------------------
-- SafeCopy :unamused:
----------------------------------------------------------------------------

deriveSafeCopySimple 0 'base ''SystemTag
deriveSafeCopySimple 0 'base ''UpdateData
deriveSafeCopySimple 0 'base ''UpdateProposal
deriveSafeCopySimple 0 'base ''UpdateVote
deriveSafeCopySimple 0 'base ''UpdatePayload
