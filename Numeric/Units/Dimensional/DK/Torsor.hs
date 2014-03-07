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

module Numeric.Units.Dimensional.DK.Torsor where

import Numeric.Units.Dimensional.DK.Prelude
import Data.AffineSpace
import Data.AdditiveGroup

newtype TorsorQuantity d a = TorsorQuantity (Quantity d a)

instance (Num a) => AffineSpace (TorsorQuantity d a) where
  type Diff (TorsorQuantity d a) = Quantity d a
  (TorsorQuantity x) .-. (TorsorQuantity y) = x - y
  (TorsorQuantity x) .+^ y = TorsorQuantity (x + y)

instance (Num a) => AdditiveGroup (Quantity d a) where
  zeroV = _0
  (^+^) = (+)
  negateV = negate
