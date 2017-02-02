{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Define scales for absolute temperature.  -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Numeric.Units.Dimensional.DK.AbsoluteTemperature where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.DK.VectorSpace() -- import instances
import Data.AffineSpace
import qualified Prelude ((-))

newtype AbsoluteTemperature a = AbsoluteTemperature (ThermodynamicTemperature a)
  deriving (Eq,Ord,Show)

instance (Num a) => AffineSpace (AbsoluteTemperature a) where
  type Diff (AbsoluteTemperature a) = ThermodynamicTemperature a
  (AbsoluteTemperature  x) .-. (AbsoluteTemperature y) = x - y
  (AbsoluteTemperature  x) .+^ y = AbsoluteTemperature  (x + y)

absoluteZero :: (Num a) => AbsoluteTemperature a
absoluteZero = AbsoluteTemperature _0

freezingPointOfWater :: (Fractional a) => AbsoluteTemperature a
freezingPointOfWater = absoluteZero .+^ (273.15 *~ degreeCelsius)

fromCelsius :: (Fractional a) => a -> AbsoluteTemperature a
fromCelsius t = freezingPointOfWater .+^ (t *~ degreeCelsius)

toCelsius :: (Fractional a, Ord a) => AbsoluteTemperature a -> a
toCelsius (AbsoluteTemperature t) | t >= _0 = t /~ degreeCelsius
                                  | otherwise = error "Negative absolute temperature."

fromFahrenheit :: (Fractional a) => a -> AbsoluteTemperature a
fromFahrenheit t = freezingPointOfWater .+^ ((t Prelude.- 32) *~ degreeFahrenheit)

toFahrenheit :: (Fractional a, Ord a) => AbsoluteTemperature a -> a
toFahrenheit (AbsoluteTemperature t) | t >= _0 = t /~ degreeFahrenheit
                                     | otherwise = error "Negative absolute temperature."
