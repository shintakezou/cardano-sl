module Pos.Core.Types.Slotting
       (
        -- * Slotting
         EpochIndex (..)
       , HasEpochIndex (..)
       , FlatSlotId
       , LocalSlotIndex (..)
       , SlotId (..)
       , EpochOrSlot (..)
       , HasEpochOrSlot (..)
       , slotIdF
       , epochOrSlot
       ) where

import           Control.Lens        (Getter, to)
import           Data.Ix             (Ix)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, int, ords, (%))
import           Universum

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Hashable, Bounded, Typeable, NFData)

instance Buildable EpochIndex where
    build = bprint ("epoch #"%int)

-- instance Buildable (EpochIndex,EpochIndex) where
--     build = bprint ("epochIndices: "%pairF)

-- | Class for something that has 'EpochIndex'.
class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Buildable, Typeable, NFData)

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") siSlot siEpoch

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq, Generic, NFData)

-- | Apply one of the function depending on content of EpochOrSlot.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

instance Ord EpochOrSlot where
    compare (EpochOrSlot e1) (EpochOrSlot e2) = case (e1,e2) of
        (Left s1, Left s2)                      -> compare s1 s2
        (Right s1, Left s2) | (siEpoch s1) < s2 -> LT
                            | otherwise         -> GT
        (Left s1, Right s2) | s1 > (siEpoch s2) -> GT
                            | otherwise         -> LT
        (Right s1, Right s2)
            | siEpoch s1 == siEpoch s2 -> siSlot s1 `compare` siSlot s2
            | otherwise -> siEpoch s1 `compare` siEpoch s2

instance Buildable EpochOrSlot where
    build = either Buildable.build Buildable.build . unEpochOrSlot

class HasEpochOrSlot a where
    _getEpochOrSlot :: a -> Either EpochIndex SlotId
    getEpochOrSlot :: a -> EpochOrSlot
    getEpochOrSlot = EpochOrSlot . _getEpochOrSlot
    epochOrSlotG :: Getter a EpochOrSlot
    epochOrSlotG = to getEpochOrSlot

instance HasEpochOrSlot EpochIndex where
    _getEpochOrSlot = Left

instance HasEpochOrSlot SlotId where
    _getEpochOrSlot = Right

instance NFData SlotId
