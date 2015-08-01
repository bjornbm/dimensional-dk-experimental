% Introducing dimensional-dk:
  Statically Checked Physical Dimensions for Haskell
% Björn Buckwalter & Doug McClean
% June 2015

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
  units where raw numeric values enter/leave the Haskell program

## Existing Solutions

- units (Richard Eisenberg)
- uom-plugin (Adam Gundry)
- dimensional, dimensional-tf (Björn Buckwalter)

# Why dimensional-dk?

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
    - Choose display units with `showIn` (not yet in master)
    - `Show` instance defaults to SI base units
- Exact Conversion Factors Using exact-pi (not yet in master)

## What Goodies Do We Have?

- Ultra-Minimal Dependencies
- No Need for TH
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
    - Provided as an orphan in `Numeric.Units.Dimensional.DK.Functor`

## What Don't We Have?

- A benchmark suite
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
cabal install dimensional-dk -j
```

## Getting Started

```haskell
import Numeric.Units.Dimensional.DK.Prelude

```

## Named Units

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

## dimensional-dk-experimental

A grab bag of goodies with extra dependencies

. . .

> - CODATA Values (not to be confused with codata...)
> - `AnyQuantity` and `AnyUnit` for quantities with statically unknown dimensions
> - Absolute temperatures
> - dimensional-dk plus:
    - time
    - ad (for scalar functions only at this point)
    - vector-space
    - linear (with the orphan `Functor` instance...)
    - HaTeX (and siunitx)

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

## Linear Algebra

An n * m matrix doesn't have n * m independent choices of dimension, it only
has n + m - 1. You can multiple A and B only when the relationship between the dimensions of
the columns of A is the inverse of the relationship between the dimensions of the rows of B.

We have a library that models this, but it isn't particularly useful without the typechecker plugin
because only monomorphic uses of it are checked.

If we can fix it up it will be very useful for control engineering problems.

## Contributing

Suggestions and pull requests are welcome.

Issue tracker and source repository are at:

https://github.com/bjornbm/dimensional-dk

# Questions
