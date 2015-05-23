% Introducing dimensional-dk
  Statically Checked Physical Dimensions for Haskells
% Björn Buckwalter & Doug McClean
% June 2015

## Why Check Dimensions?

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
    - Choose display units with `showIn`
    - `Show` instance defaults to SI base units
- Exact Conversion Factors Using exact-pi

## What Goodies Do We Have?

- Ultra-Minimal Dependencies
- No Need for TH
    - Even to define new units

## What Don't We Have?

- Custom dimensions or polymorphism over basis
    - No frogs / square mile
    - You have to live with our decision not to encode angle as a dimension
    - CGS ESU units are treated as equivalents in SI basis

## What Don't We Have?

- Torsors
    - No absolute temperatures, absolute times, etc.
    - See dimensional-dk-experimental

## What Don't We Have?

- A `Functor` instance
    - Intentionally omitted, since it can be used to break scale-invariance
    - Provided as an orphan in `Numeric.Units.Dimensional.DK.Functor`

# Examples

# Internals

# Ecosystem

## dimensional-dk-experimental

## igrf-dimensional

## atmos-dimensional

# Future Work

## Linear Algebra
