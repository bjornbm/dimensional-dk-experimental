{-# OPTIONS_HADDOCK show-extensions #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Douglas McClean
   License    : BSD3

   Maintainer : douglas.mcclean@gmail.com
   Stability  : Experimental
   Portability: GHC only

= Summary

A type checker plugin for GHC that can perform unification in the Abelian
group of types of kind 'Numeric.Units.Dimensional.DK.Dimensions.TypeLevel.Dimension'
under 'Numeric.Units.Dimensional.DK.Dimensions.TypeLevel.*'.

-}
module Numeric.Units.Dimensional.DK.Solver
(
  plugin
)
where

-- GHC API
import Outputable (Outputable (..), (<+>), ($$), text)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcPluginIO, tcPluginTrace, zonkCt)
import TcRnTypes  (Ct, TcPlugin (..), TcPluginResult(..), ctEvidence, ctEvPred,
                   ctPred, ctLoc, isGiven, isWanted, mkNonCanonical)
import TcType     (mkEqPred, typeKind)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred), Type, TyVar,
                   classifyPredType, mkTyVarTy)

-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.Units.Dimensional.DK.Solver \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Nothing }
