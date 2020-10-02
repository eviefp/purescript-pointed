# Pointed

This library is still WIP. I literally just started. DO NOT USE YET. I plan to
release it via 'package-set'/spago within a week or so.

This is an (almost) direct translation of Emily Pillmore's excellent
[smash](https://hackage.haskell.org/package/smash) Haskell library.

## TODO for first release

- distributivity/associativity/etc functions for Smash and Wedge
- converting functions between Smash, Wedge, and Can
- documentation for Smash and Wedge
- tests

## Ideas for the future

Can we make a _pointed_ generic type? That is, a `Pointed xs`, which has all
types in `xs` as `Maybe`s, with an extra maybe on the top. Might be worth
investigating.
