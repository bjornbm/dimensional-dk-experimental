{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Units.Dimensional.DK.Composite 
(
  CompositeUnit,
  showIn',
  split,
  degreeMinuteSecond,
  degreeMinute,
  footInch,
  poundOunce
) where

import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK.NonSI

-- non-empty list of units
data CompositeUnit d a = Atomic (Unit 'NonMetric d a)
                       | Composite (Unit 'NonMetric d a) (CompositeUnit d a)

showIn' :: (KnownDimension d, Ord a, Show a, RealFrac a) => CompositeUnit d a -> Quantity d a -> String
showIn' (Atomic u)       q = showIn u q
showIn' (Composite u u') q = let (n, r) = split u q
                              in (show n) ++ " " ++ (show . name $ u) ++ " " ++ (showIn' u' r)

degreeMinuteSecond :: (Floating a) => CompositeUnit DPlaneAngle a
degreeMinuteSecond = Composite degree (Composite arcminute (Atomic arcsecond))

degreeMinute :: (Floating a) => CompositeUnit DPlaneAngle a
degreeMinute = Composite degree (Atomic arcminute)

footInch :: (Fractional a) => CompositeUnit DLength a
footInch = Composite foot (Atomic inch)

poundOunce :: (Fractional a) => CompositeUnit DMass a
poundOunce = Composite poundMass (Atomic ounce)

-- | Splits a 'Quantity' into an integer multiple of the supplied 'Unit' and a remainder quantity.
--
-- Note that the sign of the remainder will always be positive.
split :: (RealFrac a, Integral b) => Unit m d a -> Quantity d a -> (b, Quantity d a)
split u x = let (n, r) = properFraction $ x /~ u
             in (n, abs $ r *~ u)
