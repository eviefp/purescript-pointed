# Pointed

This library is still WIP. I literally just started. DO NOT USE YET. I plan to
release it via 'package-set'/spago within a week or so.

This is an (almost) direct translation of Emily Pillmore's excellent
[smash](https://hackage.haskell.org/package/smash) Haskell library.

## TODO for first release

- [x] distributivity/associativity/etc functions for Smash and Wedge
    - [x] Wedge
    - [x] Smash
- [x] conversion functions with eithers and maybes
    - [x] Can
    - [x] Wedge
    - [x] Smash
- [x] conversion functions between Smash, Wedge, and Can
- [ ] documentation for Smash, Wedge, and the conversion functions
    - [x] Can
    - [x] Wedge
    - [x] Smash
    - [ ] Convert
    - [ ] Distributivity

## Ideas for the future

Can we make a _pointed_ generic type? That is, a `Pointed xs`, which has all
types in `xs` as `Maybe`s, with an extra maybe on the top. Might be worth
investigating.

Add tests!
