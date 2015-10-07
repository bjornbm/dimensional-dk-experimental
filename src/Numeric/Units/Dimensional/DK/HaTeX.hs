{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Units.Dimensional.DK.HaTeX

where

import Text.LaTeX.Base
import Numeric.Units.Dimensional.DK

instance (Texy v, KnownDimension d, Fractional v) => Texy (Quantity d v) where
  texy val = texy (val /~ siUnit)
