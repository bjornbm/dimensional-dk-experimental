module Numeric.Units.Dimensional.DK.Solver.Unification

where

import Prelude hiding ((*), (/), (^), recip)

import Numeric.Units.Dimensional.DK.Solver.NormalForm
import Numeric.Units.Dimensional.DK.Solver.SpecialTypes
import Numeric.Units.Dimensional.DK.Solver.TcPluginExtras

-- GHC API
import FastString
import Name
import Outputable
import TcPluginM
import TcRnTypes (Ct)
import TcRnMonad ( isGiven, ctEvidence )
import TcType
import Type
import Var

-- | A substitution is essentially a list of (variable, dimension) pairs,
-- but we keep the original 'Ct' constraint that lead to the substitution being
-- made, for use when turning the substitution back into constraints.
type TySubst = [SubstItem]

-- si here stands for substitution item, not Système International d'Unités
data SubstItem = SubstItem { siVar     :: TyVar
                           , siDim    :: NormDim
                           , siCt     ::  Ct
                           }

instance Outputable SubstItem where
  ppr si = ppr (siVar si) <+> text " := " <+> ppr (siDim si) <+> text "  {" <+> ppr (siCt si) <+> text "}"

-- | Apply a substitution to a single normalized dimension.
substsDim :: TySubst -> NormDim -> NormDim
substsDim []       d = d
substsDim (si : s) d = substsDim s (substDim (siVar si) (siDim si) d)

-- | Compose two substitutions.
substsSubst :: TySubst -> TySubst -> TySubst
substsSubst s = map $ \si -> si { siDim = substsDim s (siDim si) }

substsDimEquality :: TySubst -> DimEquality -> DimEquality
substsDimEquality s (ct, u, v) = (ct, substsDim s u, substsDim s v)

-- | Extends a substitution by adding an item.
extendSubst :: SubstItem -> TySubst -> TySubst
extendSubst si s = si : substsSubst [si] s

-- | Possible results of unifying a single pair of dimensions.
-- In the non-failing cases we return a substitution and a list
-- of fresh type variables that were created.
data UnificationResult = Win [TyVar] TySubst TySubst
                       | Draw [TyVar] TySubst TySubst
                       | Lose

instance Outputable UnificationResult where
  ppr (Win tvs subst unsubst)  = text "Win" <+> ppr tvs <+> ppr subst <+> ppr unsubst
  ppr (Draw tvs subst unsubst) = text "Draw" <+> ppr tvs <+> ppr subst <+> ppr unsubst
  ppr Lose                     = text "Lose"

-- | Attempt to unify two normalised units to produce a unifying
-- substitution.  The 'Ct' is the equality between the non-normalised
-- (and perhaps less substituted) unit type expressions.
unifyUnits :: Definitions -> DimEquality -> TcPluginM UnificationResult
unifyUnits uds (ct, u0, v0) = do tcPluginTrace "unifyUnits" (ppr u0 $$ ppr v0)
                                 unifyOne uds ct [] [] [] (u0 / v0)

unifyOne :: Definitions -> Ct -> [TyVar] -> TySubst -> TySubst -> NormDim -> TcPluginM UnificationResult
unifyOne uds ct tvs subst unsubst u
      | isOne u           = return $ Win tvs subst unsubst
      | isConstant u      = return   Lose
      | otherwise         = tcPluginTrace "unifyOne" (ppr u) >> go [] (ascending u)

      where
        go :: [(Atom, Integer)] -> [(Atom, Integer)] -> TcPluginM UnificationResult
        go _  []                       = return $ Draw tvs subst unsubst
        go ls (at@(VarAtom a, i) : xs) = do
            tch <- if given_mode then return True else isTouchableTcPluginM a
            let r = divideExponents (-i) $ leftover a u
            case () of
                () | tch && divisible i u -> return $ if occurs a r then Draw tvs subst unsubst
                                                                    else Win tvs (extendSubst (SubstItem a r ct) subst) unsubst
                   | tch && any (not . isBase . fst) xs -> do beta <- newUnitVar
                                                              let subst'   = extendSubst (SubstItem a    (var beta * r) ct) subst
                                                                  unsubst' = extendSubst (SubstItem beta (var a    / r) ct) unsubst
                                                              unifyOne uds ct (beta:tvs) subst' unsubst' $ substDim a (var beta * r) u
                   | otherwise            -> go (at:ls) xs

        go ls (at@(FamAtom f tys, i) : xs) = do
          mb <- matchFam f tys
          case normaliseDim uds . snd =<< mb of
            Just v  -> unifyOne uds ct tvs subst unsubst $ mkNormDim (ls ++ xs) * v ^ i
            Nothing -> go (at:ls) xs
        go ls (at@(BaseAtom  _, _) : xs) = go (at:ls) xs


        given_mode = isGiven (ctEvidence ct)

        newUnitVar | given_mode = newSkolemTyVar $ dimKind uds
                   | otherwise  = newFlexiTyVar  $ dimKind uds

        newSkolemTyVar kind = do
            x <- newUnique
            let name = mkSysTvName x (fsLit "beta")
            return $ mkTcTyVar name kind vanillaSkolemTv


type DimEquality = (Ct, NormDim, NormDim)

data SimplifyState
  = SimplifyState { simplifyFreshVars :: [TyVar]
                  , simplifySubst     :: TySubst
                  , simplifyUnsubst   :: TySubst
                  , simplifySolved    :: [DimEquality]
                  , simplifyStuck     :: [DimEquality]
                  }

instance Outputable SimplifyState where
  ppr ss = text "fresh   = " <+> ppr (simplifyFreshVars ss)
        $$ text "subst   = " <+> ppr (simplifySubst     ss)
        $$ text "unsubst = " <+> ppr (simplifyUnsubst   ss)
        $$ text "solved  = " <+> ppr (simplifySolved    ss)
        $$ text "stuck   = " <+> ppr (simplifyStuck     ss)

initialState :: SimplifyState
initialState = SimplifyState [] [] [] [] []

data SimplifyResult
  = Simplified SimplifyState
  | Impossible { simplifyImpossible :: DimEquality
               , simplifyRemaining  :: [DimEquality]
               }

instance Outputable SimplifyResult where
  ppr (Simplified ss)     = text "Simplified" $$ ppr ss
  ppr (Impossible eq eqs) = text "Impossible" <+> ppr eq <+> ppr eqs

simplifyUnits :: Definitions -> [DimEquality] -> TcPluginM SimplifyResult
simplifyUnits uds eqs0 = tcPluginTrace "simplifyUnits" (ppr eqs0) >> simples initialState eqs0
  where
    simples :: SimplifyState -> [DimEquality] -> TcPluginM SimplifyResult
    simples ss [] = return $ Simplified ss
    simples ss (eq:eqs) = do
        ur <- unifyUnits uds (substsDimEquality (simplifySubst ss) eq)
        tcPluginTrace "unifyUnits result" (ppr ur)
        case ur of
          Win  tvs subst unsubst -> let (ss', xs) = win eq tvs subst unsubst ss
                                    in simples ss' (xs ++ eqs)
          Draw _   []    _       -> simples (addStuck eq ss) eqs
          Draw tvs subst unsubst -> let (ss', xs) = draw eq tvs subst unsubst ss
                                    in simples ss' (xs ++ eqs)
          Lose                   -> return Impossible { simplifyImpossible = eq
                                                      , simplifyRemaining  = simplifyStuck ss ++ eqs }

win :: DimEquality -> [TyVar] -> TySubst -> TySubst -> SimplifyState -> (SimplifyState, [DimEquality])
win eq tvs subst unsubst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifyUnsubst   = substsSubst unsubst (simplifyUnsubst ss) ++ unsubst
                  , simplifySolved    = eq : simplifySolved ss
                  , simplifyStuck     = []
                  }
  , simplifyStuck ss )

draw :: DimEquality -> [TyVar] -> TySubst -> TySubst -> SimplifyState -> (SimplifyState, [DimEquality])
draw eq tvs subst unsubst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifyUnsubst   = substsSubst unsubst (simplifyUnsubst ss) ++ unsubst
                  , simplifySolved    = simplifySolved ss
                  , simplifyStuck     = [eq]
                  }
  , simplifyStuck ss )

addStuck :: DimEquality -> SimplifyState -> SimplifyState
addStuck eq ss = ss { simplifyStuck = eq : simplifyStuck ss }
