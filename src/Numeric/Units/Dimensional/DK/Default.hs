{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Units.Dimensional.DK.Default where

import Data.Default
import Numeric.Units.Dimensional
-- import Numeric.Units.Dimensional.Coercion

-- instance Default v => Default (Quantity d v) where
--   def = coerce (def :: v)

-- instance (KnownDimension d, Num v, Default v) => Default (Quantity d v) where
--   def =  def *~ siUnit

instance Num v => Default (Quantity d v) where
  def = _0

-- TODO Chose which of the above instances to use. Also possi
