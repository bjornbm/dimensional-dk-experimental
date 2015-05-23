% Introducing dimensional-dk
  Statically Checked Physical Dimensions for Haskells
% Björn Buckwalter & Doug McClean
% June 2015

## Why check dimensions?

## Existing Solutions

- units (Richard Eisenberg)
- uom-plugin (Adam Gundry)
- dimensional, dimensional-tf (Björn Buckwalter)

## Why choose dimensional-dk

- Type-Level and Term-Level Dimensions
- Dimensional Arithmetic
- Convenient Quantity Synonyms
    - `type DLength = 'Dim 1 0 0 0 0 0 0` (morally)
    - `type Length a = Quantity DLength a`
    - `type Capacitance a = ...`
- Pretty Printing
    - Choose display units with `showIn`
    - `Show` instance defaults to SI base units
- Ultra-Minimal Dependencies
- No Need for TH
    - Even to define new units

## What don't we have?

- Custom dimensions or polymorphism over basis
    - No frogs / square mile
    - You have to live with our decision not to encode angle as a dimension
    - CGS units are treated as equivalents in SI basis

## What don't we have?

- Torsors
    - No absolute temperatures, absolute times, etc.
    - See dimensional-dk-experimental

## What don't we have?

- A `Functor` instance
    - Intentionally omitted, since it can be used to break scale-invariance
    - Provided as an orphan in `Numeric.Units.Dimensional.DK.Functor`
