module Pos.Core.Types.Coin
       (
         -- * Coin
         Coin
       , CoinPortion
       , coinF
       , unsafeGetCoin
       , getCoinPortion
       , mkCoin
       , coinPortionDenominator
       , mkCoinPortion
       , unsafeCoinPortionFromDouble
       ) where

import           Data.Data           (Data)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, formatToString, int, (%))
import           Universum

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin maxCoinVal

-- | Maximal possible value of 'Coin'.
maxCoinVal :: Word64
maxCoinVal = 45000000000000000

-- | Make Coin from Word64.
mkCoin :: Word64 -> Coin
mkCoin = Coin
{-# INLINE mkCoin #-}

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

-- | CoinPortion is some portion of Coin, it must be in [0 .. coinPortionDenominator].
-- Main usage of it is multiplication with Coin. Usually it's needed to
-- determine some threshold expressed as portion of total stake.
newtype CoinPortion = CoinPortion
    { getCoinPortion :: Word64
    } deriving (Show, Ord, Eq, Generic, Typeable, NFData)

-- | Denominator used by 'CoinPortion'.
coinPortionDenominator :: Word64
coinPortionDenominator = (10 :: Word64) ^ (15 :: Word64)

-- | Make 'CoinPortion' from 'Word64' checking whether it is not greater
-- than 'coinPortionDenominator'.
mkCoinPortion
    :: MonadFail m
    => Word64 -> m CoinPortion
mkCoinPortion x
    | x <= coinPortionDenominator = pure $ CoinPortion x
    | otherwise = fail err
  where
    err =
        formatToString
            ("mkCoinPortion: value is greater than coinPortionDenominator: "
            %int) x

-- | Make CoinPortion from Double. Caller must ensure that value is in [0 .. 1].
-- Internally 'CoinPortion' stores 'Word64' which is divided by 'coinPortionDenominator'
-- to get actual value. So some rounding may take place.
unsafeCoinPortionFromDouble :: Double -> CoinPortion
unsafeCoinPortionFromDouble x
    | 0 <= x && x <= 1 = CoinPortion v
    | otherwise = panic "unsafeCoinPortionFromDouble: double not in [0, 1]"
  where
    v = round $ realToFrac coinPortionDenominator * x
{-# INLINE unsafeCoinPortionFromDouble #-}
