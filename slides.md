% Introducing dimensional-dk
  Statically Checked Physical Dimensions for Haskell
% Björn Buckwalter & Doug McClean
% 2015
% pandoc slides.md -f markdown -t beamer -o slides.html --standalone

## Why check dimensions?

## Existing Solutions

- units (Richard Eisenberg)
- uom-plugin (Adam Gundry)
- dimensional, dimensional-tf (Björn Buckwalter)

## Why choose dimensional-dk

- Type-Level and Term-Level Dimensions
- Comprehensive
- 

## What don't we have?

- Custom dimensions or polymorphism over basis
    - No frogs / square mile
    - You have to live with our decision not to encode angle as a dimension
    - CGS units are treated as equivalents in SI basis
- Torsors
    - No absolute temperatures, absolute times, etc.
    - See dimensional-dk-experimental
- A `Functor` instance
    - Intentionally omitted, since it can be used to break scale-invariance
    - Provided as an orphan in `Numeric.Units.Dimensional.DK.Functor`