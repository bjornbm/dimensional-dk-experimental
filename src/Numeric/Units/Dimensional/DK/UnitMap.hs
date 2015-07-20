{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Define .  -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Numeric.Units.Dimensional.DK.UnitMap where

import Numeric.Units.Dimensional.DK.Prelude hiding (lookup)
import Numeric.Units.Dimensional.DK.Dynamic
import qualified Data.Map as M
import Data.Proxy
import Data.Maybe (fromMaybe)

newtype UnitMap v = UnitMap (M.Map Dimension' (AnyUnit v))
  deriving (Show)

empty :: UnitMap v
empty = UnitMap (M.empty)

insert :: (KnownDimension d, Fractional v) => Unit a d v -> UnitMap v -> UnitMap v
insert = insertAny . demoteUnit

insertAny :: AnyUnit v -> UnitMap v -> UnitMap v
insertAny u (UnitMap m) = UnitMap $ (M.insert (getSIBasis u) u) m

lookup :: forall d v.(KnownDimension d, Fractional v) => UnitMap v -> Maybe (Unit NotPrefixable d v)
lookup m = lookupAny dim m >>= promoteUnit
         where dim = toSIBasis (Proxy :: Proxy d)

lookupAny :: Dimension' -> UnitMap v -> Maybe (AnyUnit v)
lookupAny d (UnitMap m) = M.lookup d m

type UnitScheme v = (forall d.(KnownDimension d, Fractional v) => Unit NotPrefixable d v)

lookupWithDefault :: forall v.(Fractional v) => UnitScheme v -> UnitMap v -> UnitScheme v
lookupWithDefault d m = fromMaybe d (lookup m)

lookup' :: (Fractional v) => UnitMap v -> UnitScheme v
lookup' = lookupWithDefault siUnit

showUsing :: (KnownDimension d, Show v, Fractional v) => UnitScheme v -> Quantity d v -> String
showUsing scheme = showIn scheme -- for some reason this won't type check if you eta convert?
-- this and UnitScheme should probably migrate to dimensional-dk

{-
From table 3a of the NIST guide, copied from SIUnits module:

> hertz :: Fractional a => Unit Atomic DFrequency a
> newton :: Fractional a => Unit Atomic DForce a
> pascal :: Fractional a => Unit Atomic DPressure a
> joule :: Fractional a => Unit Atomic DEnergy a
> watt :: Fractional a => Unit Atomic DPower a
> coulomb :: Fractional a => Unit Atomic DElectricCharge a
> volt :: Fractional a => Unit Atomic DElectricPotential a
> farad :: Fractional a => Unit Atomic DCapacitance a
> ohm :: Fractional a => Unit Atomic DElectricResistance a
> siemens :: Fractional a => Unit Atomic DElectricConductance a
> weber :: Fractional a => Unit Atomic DMagneticFlux a
> tesla :: Fractional a => Unit Atomic DMagneticFluxDensity a
> henry :: Fractional a => Unit Atomic DInductance a
> lumen :: Fractional a => Unit Atomic DLuminousFlux a
> lux :: Fractional a => Unit Atomic DIlluminance a

-}

exampleSI :: (Fractional v) => UnitMap v
exampleSI = id
          $ insert hertz
          $ insert newton
          $ insert pascal
          $ insert joule
          $ insert watt
          $ insert coulomb
          $ insert volt
          $ insert farad
          $ insert ohm
          $ insert siemens
          $ insert weber
          $ insert tesla
          $ insert henry
          $ insert lumen
          $ insert lux
          $ insert candela
          $ insert mole
          $ insert kelvin
          $ insert ampere
          $ insert second
          $ insert (kilo gram)
          $ insert meter
          $ empty
