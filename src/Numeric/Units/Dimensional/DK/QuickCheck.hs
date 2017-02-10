{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}


module Numeric.Units.Dimensional.DK.QuickCheck where

import Test.QuickCheck hiding (property)-- (Arbitrary, arbitrary, (==>))
import Data.Proxy
import GHC.TypeLits
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Coercion
import qualified Prelude


-- TODO replace 'D' with 'Q' in `NonZeroD` et al?


-- Arbitrary instance for `Quantity`
deriving instance Arbitrary a => Arbitrary (Quantity d a)


-- Newtypes to guarantee properties of generated quantities
-- --------------------------------------------------------

-- | @NonZeroD x@ has an 'Test.QuickCheck.Arbitrary' instance that
  -- guarantees that @x \/= 0@.
  --
  -- prop> \(NonZeroD x) -> x /= (_0 :: Length Double)
newtype NonZeroD d a = NonZeroD { getNonZeroD :: Quantity d a } deriving (Eq)

instance (Arbitrary a, Eq a, Num a) => Arbitrary (NonZeroD d a) where
  arbitrary = NonZeroD <$> suchThat arbitrary (/= _0)

deriving instance (KnownDimension d, Real a, Show a) => Show (NonZeroD d a)

-- | @PositiveD x@ has an 'Test.QuickCheck.Arbitrary' instance that
  -- guarantees that @x \> 0@.
  --
  -- prop> \(PositiveD x) -> x > (_0 :: Length Double)
newtype PositiveD d a = PositiveD { getPositiveD :: Quantity d a }  deriving (Eq)

instance (Arbitrary a, Ord a, Num a) => Arbitrary (PositiveD d a) where
  arbitrary = PositiveD <$> suchThat arbitrary (> _0)

deriving instance (KnownDimension d, Real a, Show a) => Show (PositiveD d a)

-- | @NonNegativeQ x@ has an 'Test.QuickCheck.Arbitrary' instance that
  -- guarantees that @x \>= 0@.
  --
  -- prop> \(NonNegativeD x) -> x >= (_0 :: Length Double)
newtype NonNegativeD d a = NonNegativeD { getNonNegativeD :: Quantity d a }  deriving (Eq)

instance (Arbitrary a, Ord a, Num a) => Arbitrary (NonNegativeD d a) where
  arbitrary = NonNegativeD <$> suchThat arbitrary (>= _0)

deriving instance (KnownDimension d, Real a, Show a) => Show (NonNegativeD d a)

-- | @ZeroOneQ x@ has an 'Test.QuickCheck.Arbitrary' instance that
  -- guarantees that @_0 <= x < 1 *~ siUnit@.
  --
  -- prop> \(ZeroOneD x) -> _0 <= x && x < (_1 :: Dimensionless Double)
newtype ZeroOneD a = ZeroOneD { getZeroOneD :: Dimensionless a } deriving (Eq, Show)

instance (Arbitrary a, RealFrac a) => Arbitrary (ZeroOneD a) where
  arbitrary = ZeroOneD . (*~ one). snd . properFraction . Prelude.abs <$> arbitrary
    -- TODO would one ever want this for other than dimensionless?
    -- TODO so use coerce instead of (*~ one)?


-- Generators for convenience
-- --------------------------

-- | Generator for non-zero quantities.
nonZeroArbitrary :: (Arbitrary a, Eq a, Num a) => Gen (Quantity d a)
nonZeroArbitrary = getNonZeroD <$> arbitrary

-- | Generator for quantities greater than zero.
positiveArbitrary :: (Arbitrary a, Ord a, Num a) => Gen (Quantity d a)
positiveArbitrary = getPositiveD <$> arbitrary

-- | Generator for non-negative quantities.
nonNegativeArbitrary :: (Arbitrary a, Ord a, Num a) => Gen (Quantity d a)
nonNegativeArbitrary = getNonNegativeD <$> arbitrary

-- | Generator for dimensionless quantities between zero and one.
zeroOneArbitrary :: (Arbitrary a, RealFrac a) => Gen (Dimensionless a)
zeroOneArbitrary = getNonZeroD <$> arbitrary
