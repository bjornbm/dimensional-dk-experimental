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

module Numeric.Units.Dimensional.DK.Dynamic
(
  AnyQuantity
, demoteQuantity, promoteQuantity
, AnyUnit
, demoteUnit, promoteUnit
) where

import Numeric.Units.Dimensional.DK.Prelude hiding (lookup)
import Numeric.Units.Dimensional.DK.UnitNames (UnitName, abbreviation)
import qualified Data.Map as M
import Data.Proxy
import Data.Maybe (fromMaybe)

data AnyQuantity v = AnyQuantity Dimension' v
  deriving (Eq, Show) -- TODO: real show instance

instance HasDimension (AnyQuantity v) where
  getSIBasis (AnyQuantity d _) = d

demoteQuantity :: forall d v.(KnownDimension d, Fractional v) => Quantity d v -> AnyQuantity v
demoteQuantity val = AnyQuantity dim (val /~ siUnit)
                   where dim = toSIBasis (Proxy :: Proxy d)

promoteQuantity :: forall d v.(KnownDimension d, Fractional v) => AnyQuantity v -> Maybe (Quantity d v)
promoteQuantity (AnyQuantity dim val) | dim == dim' = Just $ val *~ siUnit
                                      | otherwise   = Nothing
                                                    where
                                                      dim' = toSIBasis (Proxy :: Proxy d)

data AnyUnit v = AnyUnit Dimension' UnitName v
  deriving (Eq) -- TODO: real show instance

instance (Show v) => Show (AnyUnit v) where
  show (AnyUnit dim name v) = "1 " ++ (abbreviation name) ++ " =def= " ++ (show v) ++ " of the SI base unit"

instance HasDimension (AnyUnit v) where
  getSIBasis (AnyUnit d _ _) = d

demoteUnit :: forall a d v.(KnownDimension d, Fractional v) => Unit a d v -> AnyUnit v
demoteUnit u = AnyUnit dim (name u) (u /~ siUnit)
             where dim = toSIBasis (Proxy :: Proxy d)

promoteUnit :: forall d v.(Fractional v, KnownDimension d) => AnyUnit v -> Maybe (Unit Composite d v)
promoteUnit (AnyUnit dim name val) | dim == dim' = Just $ unit name (val *~ siUnit)
                                   | otherwise   = Nothing
                                                 where dim' = toSIBasis (Proxy :: Proxy d)
