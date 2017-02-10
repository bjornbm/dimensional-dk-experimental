{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Provides (orphan) instances of 'AdditiveGroup' and 'VectorSpace' for 'Quantity's.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Units.Dimensional.DK.VectorSpace where

import Numeric.Units.Dimensional.Prelude
import Data.AdditiveGroup
import Data.VectorSpace

instance (Num a) => AdditiveGroup (Quantity d a) where
  zeroV = _0
  (^+^) = (+)
  negateV = negate

instance (Num a) => VectorSpace (Quantity d a) where
  type Scalar (Quantity d a) = Dimensionless a
  (*^) = (*)

instance (Num a) => InnerSpace (Dimensionless a) where
  (<.>) = (*)
