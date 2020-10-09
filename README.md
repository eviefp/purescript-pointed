# Pointed
[![Maintainer: vladciobanu](https://img.shields.io/badge/maintainer-vladciobanu-lightgrey.svg)](http://github.com/vladciobanu)

Pointed sum types definition, instances, and combinators.

This is an (almost) direct translation of Emily Pillmore's excellent
[smash](https://hackage.haskell.org/package/smash) Haskell library.

## Installation

Install `pointed` with [Spago][]:

```sh
spago install pointed
```

## Quick start

TODO: Add some examples here.

## Documentation

TODO: Link pursuit documentation.


If you get stuck, there are several ways to get help:

- You can [open an issue][],
- Search or start a new thread on the [Purescript Discourse][],
- Ask on the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack][] ([invite link][]).

## Contributing

You can contribute to `pointed` in several ways:

1. If you encouter a problem, or have a question, please [open an issue](https://github.com/vladciobanu/purescript-pointed/issues).
1. Add documentation or code via a pull request.
1. Improve the library in any way by opening a PR. If the change is non-trivial, it is strongly recommended you open an issue first and discuss it with the maintainer.
1. If you use this library or write other libraries using this library as a dependency, please let us know!

## TODO for first release

- [ ] Cut a release (tag)
- [ ] Add to package-sets
- [ ] Publish docs on pursuit (?)
- [ ] Add tests?

## Ideas for the future

Can we make a _pointed_ generic type? That is, a `Pointed xs`, which has all
types in `xs` as `Maybe`s, with an extra maybe on the top. Might be worth
investigating.
