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
import Data.AdditiveGroup

instance (Num a) => AdditiveGroup (Quantity d a) where
  zeroV = _0
  (^+^) = (+)
  negateV = negate
