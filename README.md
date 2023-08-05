ssz-hs
======

This is a SimpleSerialize (SSZ) library for Haskell.

It draws on Lighthouse's [`ethereum_ssz`][ethereum_ssz] for inspiration.

At this stage it's mostly for fun, although the foundations are laid, including:

- Support for basic (unbounded) lists `[a]`.
- `Encode` and `Decode` can be derived for custom types using `GHC.Generics`.
- QuickCheck tests (run `make test`).

## License

Copyright (c) 2023 Michael Sproul

Licensed under the terms of the MIT license.

[ethereum_ssz]: https://github.com/sigp/ethereum_ssz
