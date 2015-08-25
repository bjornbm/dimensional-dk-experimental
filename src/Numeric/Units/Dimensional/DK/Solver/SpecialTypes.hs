{- |
   Copyright  : Copyright (C) 2006-2015 Douglas McClean, Adam Gundry
   License    : BSD3

   Maintainer : douglas.mcclean@gmail.com
   Stability  : Experimental
   Portability: GHC only

= Summary

Machinery that allows the type checker plugin to interact with the types that it gives
special attention.

= Copyright

Extensive portions of this file were modified from Adam Gundry's uom-plugin work. His
extensive contributions and paper are gratefully acknowledged.

-}
module Numeric.Units.Dimensional.DK.Solver.SpecialTypes
(
  Definitions(..),
  normaliseDim,
  reifyDim,
  fromTypeInt,
  toTypeInt,
  isDimKind
)
where

import Prelude hiding ((*), (/), (^), recip)
import Data.Maybe
import Data.List

import Numeric.Units.Dimensional.DK.Dimensions.TermLevel hiding ((*), (/), (^), recip)
import qualified Numeric.Units.Dimensional.DK.Dimensions.TermLevel as D
import Numeric.Units.Dimensional.DK.Solver.NormalForm

-- GHC API
import TyCon
import Type
import TypeRep
import TcType

data Definitions = Definitions 
    { dimKindCon :: TyCon -- the 'Dimension' type constructor, to be promoted to a kind
    , dimTyCon :: TyCon -- the 'Dim' constructor of type 'Dimension', promoted to a type constructor
    , dimKind :: Kind -- the kind of type-level dimensions
    , mulTyCon :: TyCon -- the (*) type family
    , divTyCon :: TyCon -- the (/) type family
    , expTyCon :: TyCon -- the (^) type family
    , recipTyCon :: TyCon -- the Recip type family
    , rootTyCon :: TyCon -- the Root type family
    , oneTy :: Type -- the type 'DOne'
    , typeIntKindCon :: TyCon -- the 'TypeInt' type constructor, to be promoted to a kidn
    , typeIntKind :: Kind -- the kind of type-level integers
    , smallTypeIntegerTyCons :: [TyCon] -- the special cased small integers constructors, from Neg9 through Zero to Pos9
    , neg10MinusTyCon :: TyCon -- the Neg10Minus constructor of type 'TypeInt', promoted to a type constructor
    , pos10PlusTyCon :: TyCon -- the Pos10Plus constructor of type 'TypeInt', promoted to a type constructor
    }


-- | Is this the 'Dimension' kind?
isDimKind :: Definitions -> Kind -> Bool
isDimKind uds ty | Just (tc, _) <- tcSplitTyConApp_maybe ty = tc == dimKindCon uds
                  | otherwise                                = False

-- | Try to convert a type to a unit normal form; this does not check
-- the type has kind 'Unit', and may fail even if it does.
normaliseDim :: Definitions -> Type -> Maybe NormDim
normaliseDim uds ty | Just ty1 <- tcView ty = normaliseDim uds ty1
normaliseDim _   (TyVarTy v)               = pure $ var v
normaliseDim uds (TyConApp tc tys)
  | tc == dimTyCon      uds, ds@[Just _, Just _, Just _, Just _, Just _, Just _, Just _] <- fmap (fromTypeInt uds) tys = pure . lit . makeProduct . fmap fromJust $ ds
  | tc == mulTyCon      uds, [x, y] <- tys = (*) <$> normaliseDim uds x <*> normaliseDim uds y
  | tc == divTyCon      uds, [x, y] <- tys = (/) <$> normaliseDim uds x <*> normaliseDim uds y
  | tc == expTyCon      uds, [x, n] <- tys = (^) <$> normaliseDim uds x <*> isNumLitTy n
  | tc == recipTyCon    uds, [x]    <- tys = recip <$> normaliseDim uds x
  | isFamilyTyCon tc                       = pure $ famUnit tc tys
  where
    makeProduct :: [Integer] -> Dimension'
    makeProduct ns = foldr (D.*) dOne (zipWith (\d n -> d D.^ (fromIntegral n)) [dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity] ns)
normaliseDim _ _ = Nothing

-- | Convert a dimension normal form to a type expression of kind 'Dimension'
reifyDim :: Definitions -> NormDim -> Type
reifyDim uds u | null xs && null ys = oneTy uds
               | null ys            = foldr1 times xs
               | null xs            = oneTy uds `divide` foldr1 times ys
               | otherwise          = foldr1 times xs `divide` foldr1 times ys
  where
    (pos, neg) = partition ((> 0) . snd) $ ascending u
    xs = map fromAtom            pos
    ys = map (fromAtom . fmap negate) neg

    times  x y = mkTyConApp (mulTyCon uds) [x, y]
    divide x y = mkTyConApp (divTyCon uds) [x, y]

    fromAtom (a, n) = pow n (reifyAtom a)
    pow 1 ty = ty
    pow n ty = mkTyConApp (expTyCon uds) [ty, mkNumLitTy n]

    reifyAtom (BaseAtom d)    = mkTyConApp (dimTyCon uds) (fmap (toTypeInt uds . fromIntegral) $ asList d)
    reifyAtom (VarAtom  v)    = mkTyVarTy  v
    reifyAtom (FamAtom f tys) = mkTyConApp f tys

-- conversion to and from integer types
fromTypeInt :: Definitions -> Type -> Maybe Integer
fromTypeInt uds ty               | Just ty1 <- tcView ty                               = fromTypeInt uds ty1
fromTypeInt uds (TyConApp n [])  | Just n' <- elemIndex n (smallTypeIntegerTyCons uds) = Just (fromIntegral n' - 9)
fromTypeInt uds (TyConApp f [n]) | f == neg10MinusTyCon uds                            = (\x -> negate x - 10) <$> isNumLitTy n
                                 | f == pos10PlusTyCon uds                             = (+ 10) <$> isNumLitTy n
fromTypeInt _   _                                                                      = Nothing                                 

toTypeInt :: Definitions -> Integer -> Type
toTypeInt uds n | n < -9    = TyConApp (neg10MinusTyCon uds) [mkNumLitTy (negate n - 10)]
                | n <= 9    = TyConApp (smallTypeIntegerTyCons uds !! fromIntegral (n + 9)) []
                | otherwise = TyConApp (pos10PlusTyCon uds) [mkNumLitTy (n - 10)]
