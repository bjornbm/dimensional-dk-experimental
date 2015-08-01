module Numeric.Units.Dimensional.DK.Solver.SpecialTypes
where

import Prelude hiding ((*), (/), (^), recip)

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
    }

-- | Try to convert a type to a unit normal form; this does not check
-- the type has kind 'Unit', and may fail even if it does.
normaliseDim :: Definitions -> Type -> Maybe NormDim
normaliseDim uds ty | Just ty1 <- tcView ty = normaliseDim uds ty1
normaliseDim _   (TyVarTy v)               = pure $ var v
normaliseDim uds (TyConApp tc tys)
  | tc == dimTyCon      uds, [l, m, t, i, th, n, j] <- tys = error "parse Dim types"
  | tc == mulTyCon      uds, [x, y] <- tys = (*) <$> normaliseDim uds x <*> normaliseDim uds y
  | tc == divTyCon      uds, [x, y] <- tys = (/) <$> normaliseDim uds x <*> normaliseDim uds y
  | tc == expTyCon      uds, [x, n] <- tys = (^) <$> normaliseDim uds x <*> isNumLitTy n
  | tc == recipTyCon    uds, [x]    <- tys = recip <$> normaliseDim uds x
  | isFamilyTyCon tc                       = pure $ famUnit tc tys
normaliseDim _ _ = Nothing

-- | tc == unitOneTyCon  uds                = pure one
-- | tc == unitBaseTyCon uds, [x]    <- tys = pure $ baseUnit x