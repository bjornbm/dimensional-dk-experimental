{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Adam Gundry
   License    : BSD3

   Maintainer : douglas.mcclean@gmail.com
   Stability  : Experimental
   Portability: GHC only

= Summary

Machinery for the type checker plugin.

= Copyright

This file was taken verbatim from Adam Gundry's uom-plugin work. His
extensive contributions and paper are gratefully acknowledged.

-}
module Numeric.Units.Dimensional.DK.Solver.TcPluginExtras
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt
  ) where

import TcPluginM  ( TcPluginM )
import TcEvidence ( EvTerm )
import TcRnTypes  ( mkNonCanonical )
import TcRnMonad  ( Ct, CtLoc )
import Type       ( PredType )

import GHC.TcPluginM.Extra

#if __GLASGOW_HASKELL__ < 711
import Unique     ( Unique )
import qualified TcRnMonad
import TcPluginM ( unsafeTcPluginTcM )
#else
import TcPluginM ( newUnique )
#endif


#if __GLASGOW_HASKELL__ < 711
newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique
#endif

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
newWantedCt loc = fmap mkNonCanonical . newWanted loc

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
newGivenCt loc prd ev = fmap mkNonCanonical $ newGiven loc prd ev