{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

It is not within the scope of the dimensional ecosystem to handle
the complex task of date and time arithmetic. It is recommended to
use the time library for handling dates and using 'Time' quantities
only when time differences are involved in calculations with other
quantities. In order to convert between the 'DiffTime'
data type in the time library and @Time@ quantities we provide the
functions 'fromDiffTime' and 'toDiffTime'.  -}

module Numeric.Units.Dimensional.DK.Time where

import Numeric.Units.Dimensional.Prelude hiding (toDiffTime, fromDiffTime)
import Data.Time.Clock (DiffTime)


-- | Convert a 'DiffTime' to a 'Time' quantity.
fromDiffTime :: Fractional a => DiffTime -> Time a
fromDiffTime = changeRep . (*~ second)

-- | Convert a 'Time' quantity into a 'DiffTime'.
toDiffTime :: (Real a, Fractional a) => Time a -> DiffTime
toDiffTime = (/~ second) . changeRep
