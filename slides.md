% Introducing dimensional:
  Statically Checked Physical Dimensions for Haskell
% Björn Buckwalter & Doug McClean
% January 2016

## Why Check Dimensions?

. . .

The usual reasons:

- Mars Climate Orbiter
- Gimli Glider (Air Canada Flight 143)

. . .

Both incidents involved more than just software, but you get the idea.

## Why Check Dimensions?

- Types as Documentation
- Type system **requires** source code documentation of
  units where raw numeric values enter/leave the program

## Existing Solutions

- units (Richard Eisenberg)
- uom-plugin (Adam Gundry)
- dimensional-tf (Björn Buckwalter)

# Why dimensional?

## What Goodies Do We Have?

- Types Reflect Dimensions, Not Units
- Type-Level and Term-Level Dimensions
- Strongly Kinded

## What Goodies Do We Have?

- Dimensional Arithmetic
- Convenient Quantity Synonyms
    - `type DLength = 'Dim 1 0 0 0 0 0 0` (morally)
    - `type Length a = Quantity DLength a`
    - `type Capacitance a = ...`
- Pretty Printing
    - Choose display units with `showIn`
    - `Show` instance defaults to SI base units
- Exact Conversion Factors Using `exact-pi`

## What Goodies Do We Have?

- Ultra-Minimal Dependencies
- No Need for TH or Solver Plugins
    - Even to define new units

## What Don't We Have?

> - Custom dimensions or polymorphism over basis
    - No frogs / square mile
    - You have to live with our decision not to encode angle as a dimension, although
      doing so is potentially useful from an engineering perspective
    - CGS ESU units are treated as equivalents in SI basis

## What Don't We Have?

- Torsors
    - No absolute temperatures, absolute times, etc.
    - See dimensional-dk-experimental

## What Don't We Have?

- A `Functor` instance
    - Intentionally omitted, since it can be used to break scale-invariance

## What Don't We Have?

- A solid benchmark suite
- Appropriate `INLINE`, `SPECIALIZE`, and `RULES` pragmas arising from same

## What Don't We Have?

- A type solver plugin, like Adam Gundry's

It's very useful for code that is heavily polymorphic in dimension. For example, our attempts
to build a usable dimensionally-typed linear algebra library have been hampered by error messages of
the form:

```
Couldn't match type `((x / iv) / u) * u' 
               with `((x / iv) / x) * x'
```

I'm working on developing this, but could use some help.

# Examples

## Getting Started

```bash
cabal update
cabal install dimensional
```

## Getting Started

Here's an example word problem from the readme file:

A car travels at 60 kilometers per hour for one mile, at 50 kph for one mile, at 40 kph for one mile, and at 30 kph for one mile.

  - How many minutes does the journey take?
  - What is the average speed of the car?
  - How many seconds does the journey take, rounded up to the next whole second?

## Readme Example Continued

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module ReadmeExample where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (mile)
```
. . .
```haskell
leg :: Length Double
leg = 1 *~ mile
```
. . .
```haskell
speeds :: [Velocity Double]
speeds = [60, 50, 40, 30] *~~ (kilo meter / hour)
```

## Readme Example Continued

```haskell
timeOfJourney :: Time Double
timeOfJourney = sum $ fmap (leg /) speeds
```
. . .
```haskell
averageSpeed :: Velocity Double
averageSpeed = _4 * leg / timeOfJourney
--           = (4 *~ one) * leg / timeOfJourney
```
. . .
```haskell
wholeSeconds :: Integer
wholeSeconds = ceiling $ timeOfJourney /~ second
```

## Reading Aircraft State from FlightGear

```haskell
readState :: [Double] -> VehicleState'
readState [r, p, y, rDot, pDot, yDot, ax, ay, az, slip, as, vx, vy, vz, msl, agl, lat, lon, et, rpm, temp, statpres, dynpres]
  = VehicleState' { ... }
    where
      _orientation = quaternionFromTaitBryan (y *~ degree) (p *~ degree) (r *~ degree)
      _orientationRate = quaternionFromTaitBryan (yDot *~ degree) (pDot *~ degree) (rDot *~ degree)
      _velocity = (V3 vx vy vz) *~~ (foot / second)
      _acceleration = (V3 ax ay az) *~~ (foot / second / second)
      _sideSlip = slip *~ degree
      _airspeed = as *~ (nauticalMile / hour)
      _altitudeMSL = msl *~ foot
      _altitudeAGL = agl *~ foot
      _location = GeodeticPlace . fromJust $ lat <°> lon
      _elapsedTime = et *~ second
      _propellerSpeed = rpm *~ (revolution / minute)
      _staticPressure = statpres *~ inHg
      _dynamicPressure = dynpres *~ (poundForce / square foot)
```

## Defining Custom Units

# Internals

# Ecosystem

## dimensional-codata

- CODATA Values (not to be confused with codata...)
    - Speed of light
    - Planck constant
    - etc.

## exact-pi

```haskell
data ExactPi = Exact Integer Rational
             | Approximate (forall a.Floating a => a)

approximateValue :: Floating a => ExactPi -> a
```

> - Provides an exact representation of rational multiples of integer powers of pi
> - Provides `Num`, `Fractional`, `Floating` instances which fall back to `Approximate` where necessary
> - Non-zero such numbers form a group under multiplication
> - All exactly defined units we have encountered in practice have an exact representation
> - Universal type of `Approximate` defers computations with `pi`, `+`, etc. until after the desired result type
    has been selected.

## igrf and atmos

We have dimensionally typed wrappers around some libraries that provide physical information, for example

- igrf, which implements the International Geomagnetic Reference Field
- atmos, which implements the 1976 International Standard Atmosphere

# Future Work

## Forthcoming Version 1.1

> - Improved support for dynamic quantities
> - Improvements to unit names that are necessary for proper parsing
> - Fixed-point quantities (details on next slide)
> - User manual

## Forthcoming Fixed-Point Support

```haskell
data Variant = DQuantity
             | DUnit Metricality

type Quantity = Dimensional DQuantity
```
. . .

becomes

```haskell
data Variant = DQuantity ExactPi -- scale factor
             | DUnit Metricality

type SQuantity s = Dimensional (DQuantity s)

type Quantity = SQuantity One
```

## Forthcoming Fixed-Point Support

```haskell
import qualified GHC.TypeLits           as N
import qualified Data.ExactPi.TypeLevel as E

-- A dimensionless number with n fractional bits,
-- using a representation of type a.
type Q n a = SQuantity (E.One E./ 
             (E.ExactNatural (2 N.^ n))) DOne a
```
. . .
```haskell
-- A single-turn angle represented as
-- a signed 16-bit integer.
type Angle16 = SQuantity (E.Pi E./
               (E.ExactNatural (2 N.^ 15)))
               DPlaneAngle Int16
```
. . .
```haskell
fast_sin :: Angle16 -> Q 15 Int16
```

. . .

With Template Haskell we can do even better tricks.

## Forthcoming Fixed-Point Support

```haskell
data VehicleState = VehicleState {
  lat :: Angle32,
  lon :: Angle32,
  altitutde :: [exact| mm ] Int32,
  vnorth :: [exact| cm / s ] Int16,
  veast  :: [exact| cm / s ] Int16,
  vdown  :: [exact| cm / s ] Int16,
  elapsedTime :: [exact| ms ] Word32,
  pressure :: [exact| 0.1 Pa ] Word32
}
```

The only holdup here is some remaining work on the parser.

## Fixed-Point Arithmetic

```haskell
(+), (-)    :: (Num a) => SQuantity s d a
                       -> SQuantity s d a
                       -> SQuantity s d a
abs, negate :: (Num a) => SQuantity s d a
                       -> SQuantity s d a

epsilon :: (Integral a) => SQuantity s d a

_0 :: Num a => SQuantity s d a

pi :: (Integral a, E.KnownExactPi s) => SQuantity s DOne a
```

## Fixed-Point Arithmetic
```haskell
(*~) :: (RealFrac a, Integral b, E.MinCtxt s a) 
     => a -> Unit m d a -> SQuantity s d b
```
. . .
```haskell
rescale :: (Integral a, Integral b, 
            E.KnownExactPi s1, E.KnownExactPi s2)
        => SQuantity s1 d a -> SQuantity s2 d b
```
. . .
```haskell
rescaleVia :: (Integral a, Integral c,
               RealFrac b, Floating b,
               E.KnownExactPi s1, E.KnownExactPi s2)
           => Proxy b 
           -> SQuantity s1 d a -> SQuantity s2 d c
```

## Linear Algebra

An n * m matrix doesn't have n * m independent choices of dimension, it only
has n + m - 1. You can multiply A and B only when the relationship between the dimensions of
the columns of A is the inverse of the relationship between the dimensions of the rows of B.

We have a library that models this, but it isn't particularly useful without the typechecker plugin
because only monomorphic uses of it are checked.

If we can fix it up it will be very useful for control engineering problems.

## Contributing

Suggestions and pull requests are welcome.

Issue tracker and source repository are at:

https://github.com/bjornbm/dimensional

# Questions
