{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       ( SharedSeed (..)
       , SlotLeaders

       , ProxySigLight
       , ProxySKLight
       , ProxySigHeavy
       , ProxySKHeavy
       , ProxySKEither
       ) where

import qualified Data.Text.Buildable  as Buildable
import           Serokell.Util.Base16 (formatBase16)
import           Universum

import           Pos.Crypto           (ProxySecretKey, ProxySignature)
import           Pos.Types.Core       (EpochIndex, StakeholderId)

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
