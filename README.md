![Minter SDK header](/docs/mintersdkhead.png)

[![](https://img.shields.io/badge/license-MIT-brightgreen)](LICENSE)

## Notice

This software is in beta. At the moment, the smart contracts
that OpenMinter uses have **not** been formally audited. Please
use this software at your own risk.

## Minter SDK

Minter SDK is a set of tools and libraries to bring all functionality
around the creation, management, and sales of NFTs to any application. 

The Minter SDK will contain various functions, such as:
- Contract definitions and tests
- An NPM module that allows users to interact with tokens programmatically
- A CLI that brings SDK functions to the command line

## Implemented FA2 Contracts

### [English Auction](english_auction)
An implementation of an English auction marketplace that allows users to initiate auctions of NFTs. There are (as of writing this) 
two versions of this contract: one with administration capabilities and one without. For now, in the positive case, this means that
a configured administrator is responsible for configuring and managing access to important entrypoints. 
### [Fixed Price Sale](fixed_price_sale)
An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price. There are multiple flavors of this contract that vary along two variables: admin capabilities, and NFT sale type (fungible tokens/tez).
### [Minter Collection](minter_collection)
Implementations of various token types.


## Project Architecture

### Yarn Workspaces

The code in this repo will be organized around yarn workspaces. I.e. each module should be placed in the packages folder.

No dependencies are required other than yarn. Running `yarn` at the root directory is sufficient to initialize a project.

### Yarn Package Setup 

Each folder under packages, has it's own package.json, like any normal npm module. 

- To include another package as a dependency, use the `*` version. Example:

```
  "dependencies": {
    "@minter-sdk/contract-api-generator": "*"
  }
```

- Register each package with typescript, so that the typescript compiler can import it directly:

```
    "baseUrl": "packages",
    "paths": {
      "@minter-sdk/contact-api-generator": [
        "./contact-api-generator/src"
      ],
    },
```

- Run `yarn` again to initialize package dependencies (after adding a new package or dependency)


### Npm Packages

This project is setup to use yarn workspaces. It will produce multiple npm modules:

- `@minter-sdk/minter-cli`
- `@minter-sdk/minter-sdk`
- `@minter-sdk/contracts`

These packages will be compiled typescript and in their compiled code, each will include any local packages required. So dependent packages do not need to be published, but should be marked as private. Example:

- `@minter-sdk/contract-api-generator` is used by `minter-cli` and `minter-sdk` and the build output of each will include it's required code.



## Development Tasks

### Setup

- `yarn`

### Lint

Run eslint and auto fix errors

- `yarn run lint`

### Test

Run tests in the flextesa sandbox

- `yarn run start-sandbox`
- `yarn run test`

