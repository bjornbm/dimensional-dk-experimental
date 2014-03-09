{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Define .  -}

{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Units.Dimensional.DK.UnitMap where

import Numeric.Units.Dimensional.DK.Prelude
import qualified Data.Map as M

data AnyUnit v = AnyUnit Dimension' UnitName v
  deriving (Eq, Ord, Show)

demote :: (KnownDimension d) => Unit a d v -> AnyUnit v
demote = undefined

promote :: AnyUnit v -> Maybe (Unit Composite d v)
promote = undefined

newtype UnitMap v = UnitMap (M.Map Dimension' (AnyUnits v))
  deriving (Show)

empty :: UnitMap v
empty = UnitMap (M.empty)

insert :: (KnownDimension d) => Unit a d v -> UnitMap v -> UnitMap v
insertAny = insertAny . demote

insertAny :: AnyUnit v -> UnitMap v -> UnitMap v
insertAny u@(AnyUnit d v) (M.Map m) = M.Map (M.insert d u) m

lookup :: forall d v.(KnownDimension d) => UnitMap v -> Maybe (UnitInfo d v)
lookup m = fmap promote $ lookupAny (toSIBasis (Proxy :: Proxy d)) m

lookupAny :: Dimension' -> UnitMap v -> AnyUnit v
lookupAny d (M.Map m) = M.lookup d m

showUsing :: (KnownDimension d) => UnitMap v -> Quantity d v -> String
showUsing m q = case (lookup m) of
                  (Just u') -> showIn u' q
                  Nothing -> show q
