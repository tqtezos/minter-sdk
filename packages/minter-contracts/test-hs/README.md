# Cleveland tests on minter-sdk contracts

This folder contains:

* A Haskell library with the bindings for the contracts' data types.
* A test project with tests suites for the contracts.

## Build and test instructions

You need [Stack](http://haskellstack.org/) to build this package.

To run tests:

* Compile contracts with: `yarn compile-ligo`.
* Run `make test`.

This will launch simplified tests without a real tezos node being involved.
This can be configured, see the `Makefile` for some hints.
