module Numeric.Units.Dimensional.DK.Solver.NormalForm
( 
  Atom(..)
, NormDim
  -- * Constructors for normalized dimensions
, one, lit, var, famUnit, mkNormDim
  -- * Arithmetic on normalized dimensions
, (*), (/), (^), recip
  -- * Predicates
, isOne, isConstant, isBase, maybeConstant, occurs, divisible
  -- * Destructors
, leftover, ascending, divideExponents
  -- * Substitution
, substDim
)
where

import Prelude hiding ((*), (/), (^), recip)
import qualified Prelude as P
import qualified Data.Map.Strict as M
import Data.List (sortBy)
import Data.Ord (comparing)

import Numeric.Units.Dimensional.DK.Dimensions.TermLevel hiding ((*), (/), (^), recip)
import qualified Numeric.Units.Dimensional.DK.Dimensions.TermLevel as D

-- GHC API
import Outputable
import Type (Type, TyVar, cmpTypes, tyVarsOfTypes)
import TyCon (TyCon)
import Util (thenCmp)
import VarSet (elemVarSet)

-- | An atom in the normal form is either a variable or a
-- stuck type family application (but not one of the built-in type
-- families that correspond to group operations).
--
-- Literal dimensions are handled separately in 'NormForm'.
data Atom = BaseAtom Dimension' -- one of the base dimensions
          | VarAtom TyVar -- a type variable of kind Dimension
          | FamAtom TyCon [Type] -- a stuck type family application of a type family other than those used for dimension arithmetic

instance Eq Atom where
  a == b = compare a b == EQ

-- TODO: using cmpTypes here probably isn't ideal, but does it matter?
instance Ord Atom where
  compare (BaseAtom x)    (BaseAtom y)      = compare x y
  compare (BaseAtom _)    _                 = LT
  compare (VarAtom  _)    (BaseAtom _)      = GT
  compare (VarAtom  a)    (VarAtom  b)      = compare a b
  compare (VarAtom  _)    (FamAtom _ _)     = LT
  compare (FamAtom f tys) (FamAtom f' tys') = compare f f' `thenCmp` cmpTypes tys tys'
  compare (FamAtom _ _)   _                 = GT

instance Outputable Atom where
  ppr (BaseAtom d) = text $ show d
  ppr (VarAtom  v) = ppr v
  ppr (FamAtom tc tys) = ppr tc <> text " " <> ppr tys

-- | A dimension expressed in normal form
newtype NormDim = NormDim { atoms :: M.Map Atom Integer }
  deriving Eq

instance Outputable NormDim where
  ppr = ppr . M.map show . atoms

-- | The normalized dimension of dimensionless values.
one :: NormDim
one = NormDim M.empty

-- | Construct a normalized dimension from a literal dimension.
lit :: Dimension' -> NormDim
lit d = mkNormDim $ zip (fmap BaseAtom baseDimensions) (fmap fromIntegral $ asList d)
  where
    baseDimensions = [dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity]

-- | Construct a normalized dimension from a single atom.
atom :: Atom -> NormDim
atom a = NormDim $ M.singleton a 1

-- | Construct a normalized dimension from a type variable.
var :: TyVar -> NormDim
var = atom . VarAtom

-- | Construct a normalised dimension from a stuck type family application:
-- this must not be one of the built-in type families!
-- TODO: verify that it isn't
famUnit :: TyCon -> [Type] -> NormDim
famUnit tc ts = atom $ FamAtom tc ts

-- | Construct a normalised dimension from a literal dimension and a list of atom-exponent pairs
mkNormDim :: [(Atom, Integer)] -> NormDim
mkNormDim = mkNormDimMap . M.fromList

-- | Construct a normalised unit from a literal dimension and an atom-exponent map, applying
-- the signed multiset invariant
mkNormDimMap :: M.Map Atom Integer -> NormDim
mkNormDimMap as = NormDim $ M.filter (/= 0) as

-- Arithmetic on normalized dimensions.
(*) :: NormDim -> NormDim -> NormDim
(NormDim as) * (NormDim as') = mkNormDimMap $ M.unionWith (P.+) as as'

(/) :: NormDim -> NormDim -> NormDim
x / y = x * recip y

(^) :: NormDim -> Integer -> NormDim
_ ^ 0              = one
(NormDim as) ^ n = NormDim $ M.map (P.* n) as

infixl 7 *, /
infixr 8 ^

recip :: NormDim -> NormDim
recip = NormDim . M.map negate . atoms

-- | Test whether a dimension is dimensionless.
isOne :: NormDim -> Bool
isOne = null . atoms

-- | Test whether a dimension is a constant dimension.
isConstant :: NormDim -> Bool
isConstant = all isBase . M.keys . atoms

isBase :: Atom -> Bool
isBase (BaseAtom _) = True
isBase _            = False

-- | Extract the value of a normalized dimension if it is constant.
maybeConstant :: NormDim -> Maybe Dimension'
maybeConstant d | isConstant d = Just $ foldr (D.*) dOne (fmap asDim . M.assocs . atoms $ d)
                | otherwise    = Nothing
  where
    asDim (BaseAtom d', n) = d' D.^ (fromInteger n)
    asDim _                = error "Encountered non-base atom in constant normalized dimension?"

-- | Test whether all exponents in a unit are divisble by an integer
divisible :: Integer -> NormDim -> Bool
divisible i = all (\ j -> j `rem` i == 0) . atoms

-- | Divide all the exponents in a unit by an integer
divideExponents :: Integer -> NormDim -> NormDim
divideExponents i = mkNormDimMap . M.map (`quot` i) . atoms

-- | View a dimension as a list of atoms in order of ascending absolute exponent
ascending :: NormDim -> [(Atom, Integer)]
ascending = sortBy (comparing (abs . snd)) . M.toList . atoms

-- | Test whether a type variable occurs in a dimension (possibly
-- under a type family application.
occurs :: TyVar -> NormDim -> Bool
occurs a = any occursAtom . M.keys . atoms
  where
    occursAtom (VarAtom b) = a == b
    occursAtom (FamAtom _ tys) = elemVarSet a $ tyVarsOfTypes tys

-- | Drop a variable from a dimension.
leftover :: TyVar -> NormDim -> NormDim
leftover a = NormDim . M.delete (VarAtom a) . atoms

-- | Substitute the first dimension for the variable in the second dimension.
substDim :: TyVar -> NormDim -> NormDim -> NormDim
substDim a v d@(NormDim as) = case M.lookup (VarAtom a) as of
                                Nothing -> d
                                Just i -> (v ^ i) * leftover a d
