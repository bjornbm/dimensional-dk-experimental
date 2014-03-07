{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Define .  -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Numeric.Units.Dimensional.DK.AbsoluteTemperature where

import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK.Torsor
import Data.AffineSpace
import qualified Prelude ((-),(/))

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

fromFahrenheit :: (Fractional a) => a -> AbsoluteTemperature a
fromFahrenheit t = freezingPointOfWater .+^ ((t Prelude.- 32) *~ degreeFahrenheit)

-- attempting to migrate this to core dimensional-dk
degreeFahrenheit :: (Fractional a) => Unit DThermodynamicTemperature a
degreeFahrenheit = prefix (5 Prelude./ 9) degreeCelsius
