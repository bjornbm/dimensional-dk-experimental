{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Provides instances of @Data.AEq.AEq@ from the ieee754 library.
module Numeric.Units.Dimensional.AEq where

import Numeric.Units.Dimensional (Quantity)
import Numeric.Units.Dimensional.Coercion
import Data.AEq


instance AEq a => AEq (Quantity d a) where
  x ~== y = (coerce x :: a) ~== coerce y
