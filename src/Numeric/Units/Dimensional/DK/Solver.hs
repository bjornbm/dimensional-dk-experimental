{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RecordWildCards #-}

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

import Data.Either

import Numeric.Units.Dimensional.DK.Solver.Unification
import Numeric.Units.Dimensional.DK.Solver.SpecialTypes
import Numeric.Units.Dimensional.DK.Solver.TcPluginExtras

-- GHC API
import Plugins
import TcEvidence
import TcRnTypes
import TcType
import TcPluginM
import DataCon
import Type
import TyCon
import TypeRep
import FastString
import Outputable
import OccName ( occName, occNameFS, mkTcOcc )
import Module

import GHC.TcPluginM.Extra ( evByFiat, tracePlugin, lookupModule, lookupName )

-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.Units.Dimensional.DK.Solver \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just dimPlugin }

dimPlugin :: TcPlugin
dimPlugin = tracePlugin "dim-plugin" $ TcPlugin { tcPluginInit  = lookupDefs
                                                , tcPluginSolve = dimensionSolver
                                                , tcPluginStop  = const $ return ()
                                                }

lookupDefs :: TcPluginM Definitions
lookupDefs = do
    tmod <- lookupModule typeIntModule typeIntPackage
    dmod <- lookupModule dimModule dimPackage
    dimKindCon <- look dmod "Dimension"
    let dimTyCon = getDataCon dimKindCon "Dim"
    let dimKind = TyConApp (promoteTyCon dimKindCon) []
    mulTyCon <- look dmod "*"
    divTyCon <- look dmod "/"
    expTyCon <- look dmod "^"
    recipTyCon <- look dmod "Recip"
    rootTyCon <- look dmod "Root"
    oneTyCon <- look dmod "DOne"
    let oneTy = TyConApp oneTyCon []
    typeIntKindCon <- look tmod "TypeInt"
    let typeIntKind = TyConApp (promoteTyCon typeIntKindCon) []
    let smallTypeIntegerTyCons = fmap (getDataCon typeIntKindCon) ["Neg9", "Neg8", "Neg7", "Neg6", "Neg5", "Neg4", "Neg3", "Neg2", "Neg1", "Zero", "Pos1", "Pos2", "Pos3", "Pos4", "Pos5", "Pos6", "Pos7", "Pos8", "Pos9"]
    let neg10MinusTyCon = getDataCon typeIntKindCon "Neg10Minus"
    let pos10PlusTyCon = getDataCon typeIntKindCon "Pos10Plus"
    return $ Definitions {..}
  where
    getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> promoteDataCon d
                       _   -> error $ "lookupDefs/getDataCon: missing " ++ s

    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    typeIntModule = mkModuleName "Numeric.NumType.DK.Integers"
    typeIntPackage = fsLit "numtype-dk"
    dimModule = mkModuleName "Numeric.Units.Dimensional.DK.Dimensions.TypeLevel"
    dimPackage = fsLit "dimensional-dk"



dimensionSolver :: Definitions -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
dimensionSolver uds givens _deriveds []      = do
    zonked_cts <- mapM zonkCt givens
    let (unit_givens , _) = partitionEithers $ zipWith foo givens $ map (toDimEquality uds) zonked_cts
    case unit_givens of
      []    -> return $ TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyDims uds $ map snd unit_givens
        tcPluginTrace "dimensionSolver simplified givens only" $ ppr sr
        return $ case sr of
          -- Simplified tvs []    evs eqs -> TcPluginOk (map (solvedGiven . fst) unit_givens) []
          Simplified _    -> TcPluginOk [] []
          Impossible eq _ -> TcPluginContradiction [fromDimEquality eq]
  where
    foo :: Ct -> Either DimEquality Ct -> Either (Ct, DimEquality) Ct
    foo ct (Left x)    = Left (ct, x)
    foo _  (Right ct') = Right ct'

    -- solvedGiven ct = (ctEvTerm (ctEvidence ct), ct)


dimensionSolver uds givens _deriveds wanteds = do
  let (unit_wanteds, _) = partitionEithers $ map (toDimEquality uds) wanteds
  case unit_wanteds of
    []    -> return $ TcPluginOk [] []
    (_:_) -> do
      (unit_givens , _) <- partitionEithers . map (toDimEquality uds) <$> mapM zonkCt givens
      sr <- simplifyDims uds unit_givens
      tcPluginTrace "dimensionSolver simplified givens" $ ppr sr
      case sr of
        Impossible eq _ -> return $ TcPluginContradiction [fromDimEquality eq]
        Simplified ss   -> do sr' <- simplifyDims uds $ map (substsDimEquality (simplifySubst ss)) unit_wanteds
                              tcPluginTrace "dimensionSolver simplified wanteds" $ ppr sr'
                              case sr' of
                                Impossible eq _ -> return $ TcPluginContradiction [fromDimEquality $ substsDimEquality (simplifyUnsubst ss) eq]
                                Simplified ss'  -> TcPluginOk [ (evMagic uds ct, ct) | eq <- simplifySolved ss', let ct = fromDimEquality eq ]
                                                       <$> mapM (substItemToCt uds) (filter (isWanted . ctEvidence . siCt) (substsSubst (simplifyUnsubst ss) (simplifySubst ss')))








substItemToCt :: Definitions -> SubstItem -> TcPluginM Ct
substItemToCt uds si
      | isGiven (ctEvidence ct) = newGivenCt loc prd $ evByFiat "dimensions" ty1 ty2
      | otherwise               = newWantedCt loc prd
      where
        prd  = mkEqPred ty1 ty2
        ty1  = mkTyVarTy (siVar si)
        ty2  = reifyDim uds (siDim si)
        ct   = siCt si
        loc  = ctLoc ct

-- Extract the unit equality constraints
toDimEquality :: Definitions -> Ct -> Either DimEquality Ct
toDimEquality uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isDimKind uds (typeKind t1) || isDimKind uds (typeKind t1)
      , Just u1 <- normaliseDim uds t1
      , Just u2 <- normaliseDim uds t2 -> Left (ct, u1, u2)
    _                                   -> Right ct
{-
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds
      , Just u1 <- normaliseUnit uds t1
      , Just u2 <- normaliseUnit uds t2 -> Left (ct, u1, u2)
-}

fromDimEquality :: DimEquality -> Ct
fromDimEquality (ct, _, _) = ct

-- | Produce bogus evidence for a constraint, including actual
-- equality constraints. (and our fake '(~~)' equality constraints?)
evMagic :: Definitions -> Ct -> EvTerm
evMagic _ ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2   -> evByFiat "dimensions" t1 t2
    _                    -> error "evMagic"
{-
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds -> evByFiat "dimensions" t1 t2 `EvCast`
                                  TcCoercion (mkUnivCo (fsLit "dimensions") Representational (mkTyConApp eqTyCon [typeKind t1, t1, t2]) t)

-}