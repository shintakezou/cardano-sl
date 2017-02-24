{-# LANGUAGE ScopedTypeVariables #-}

-- | Core types. TODO: we need to have a meeting, come up with project
-- structure and follow it.

module Pos.Core.Types.Core
       (
        -- * Address
         Address (..)
       , AddrPkAttrs (..)
       , AddressHash
       , StakeholderId

       , Timestamp (..)

        -- * ChainDifficulty
       , ChainDifficulty (..)
       , HasDifficulty (..)

        -- * Version
       , ApplicationName (..)
       , BlockVersion (..)
       , NumSoftwareVersion
       , SoftwareVersion (..)

       -- * HeaderHash related types and functions
       , BlockHeaderStub
       , HeaderHash
       , HasHeaderHash (..)
       , headerHashF

       , ProxySigLight
       , ProxySKLight
       , ProxySigHeavy
       , ProxySKHeavy
       , ProxySKEither

       , SharedSeed
       , SlotLeaders
       ) where

import           Control.Lens            (Getter, to)
import           Crypto.Hash             (Blake2s_224)
import           Data.Default            (Default (..))
import           Data.Hashable           (Hashable)
import           Data.Text.Buildable     (Buildable)
import qualified Data.Text.Buildable     as Buildable
import           Data.Time.Units         (Microsecond)
import           Formatting              (Format, build)
import           Serokell.AcidState      ()
import           Serokell.Util.Base16    (formatBase16)
import           Universum

import           Pos.Core.Script         (Script)
import           Pos.Core.Types.Slotting (EpochIndex)
import           Pos.Crypto              (AbstractHash, Hash, ProxySecretKey,
                                          ProxySignature, PublicKey)
import           Pos.Data.Attributes     (Attributes)
-- TODO fix it ^


-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable, Generic)

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash      :: !(AddressHash PublicKey)
          , addrPkAttributes :: !(Attributes AddrPkAttrs) }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    | UnknownAddressType !Word8 !ByteString
    deriving (Eq, Ord, Generic, Typeable, Show)

instance NFData Address

newtype AddrPkAttrs = AddrPkAttrs
    { addrPkDerivationPath :: Maybe [Word32]
    } deriving (Eq, Ord, Show, Generic, Typeable, NFData)

instance Default AddrPkAttrs where
    def = AddrPkAttrs Nothing

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

type AddressHash = AbstractHash Blake2s_224

----------------------------------------------------------------------------
-- ChainDifficulty
----------------------------------------------------------------------------

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable, NFData)

-- | Type class for something that has 'ChainDifficulty'.
class HasDifficulty a where
    difficultyL :: Lens' a ChainDifficulty

----------------------------------------------------------------------------
-- Version
----------------------------------------------------------------------------

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, NFData)

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

instance NFData BlockVersion
instance NFData SoftwareVersion

----------------------------------------------------------------------------
-- HeaderHash
----------------------------------------------------------------------------

-- | 'Hash' of block header. This should be @Hash (BlockHeader ssc)@
-- but we don't want to have @ssc@ in 'HeaderHash' type.
type HeaderHash = Hash BlockHeaderStub
data BlockHeaderStub

-- | Specialized formatter for 'HeaderHash'.
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- | Class for something that has 'HeaderHash'.
class HasHeaderHash a where
    headerHash :: a -> HeaderHash
    headerHashG :: Getter a HeaderHash
    headerHashG = to headerHash

----------------------------------------------------------------------------
-- Proxy signatures and delegation
----------------------------------------------------------------------------

-- | Proxy signature used in csl -- holds a pair of epoch
-- indices. Block is valid if it's epoch index is inside this range.
type ProxySigLight a = ProxySignature (EpochIndex, EpochIndex) a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey (EpochIndex, EpochIndex)

-- | Simple proxy signature without ttl/epoch index
-- constraints. 'EpochIndex' inside is needed for replay attack
-- prevention.
type ProxySigHeavy a = ProxySignature EpochIndex a

-- | Correspondent SK for no-ttl proxy signature scheme.
type ProxySKHeavy = ProxySecretKey EpochIndex

-- | Some proxy secret key.
type ProxySKEither = Either ProxySKLight ProxySKHeavy

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
newtype SharedSeed = SharedSeed
    { getSharedSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, NFData, Typeable)

instance Buildable SharedSeed where
    build = formatBase16 . getSharedSeed

-- | 'NonEmpty' list of slot leaders.
type SlotLeaders = NonEmpty StakeholderId
