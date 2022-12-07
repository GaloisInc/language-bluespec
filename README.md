# `language-bluespec`

This package contains an implementation of the
[Bluespec](http://wiki.bluespec.com/) AST. In particular, this implements the
Bluespec Haskell (BH) syntax, also known as Bluespec Classic. We may add
support for the Bluespec SystemVerilog (BSV) syntax at a later date.

To our knowledge, there is no formal grammar that describes the syntax of BH or
BSV, so this package is based off of the code in the [Bluespec
compiler](https://github.com/B-Lang-org/bsc). Although the Bluespec compiler is
written in Haskell, it is not particularly simple to depend on the compiler as
a library, so this package exists to extract out the relevant compiler code
into a simple-to-use library.
