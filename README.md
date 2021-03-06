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

## Shared LIGO Code

The [shared](shared) directory contains common code shared by all sub-projects.
The FA2-related LIGO artifacts are:

- [shared/fa2](shared/fa2) - FA2 interface and standard errors definition.
- [shared/fa2/lib](shared/fa2/lib) - Helpers, various bits and pieces
  used by the FA2 implementation.
  - [shared/fa2/lib/fa2_operator_lib.mligo](shared/fa2/lib/fa2_operator_lib.mligo) -
    Helper functions to manage and validate FA2 operators.
  - [shared/fa2/lib/fa2_owner_hook_lib.mligo](shared/fa2/lib/fa2_owner_hook_lib.mligo) -
    Helper functions to support sender/receiver hooks.
- [shared/fa2_modules](shared/fa2_modules) - Modules implementing additional contract
  functionality to be mixed into the final FA2 contract.
  - [shared/fa2_modules/simple_admin_option.mligo](shared/fa2_modules/simple_admin_option.mligo) -
    Implementation of the admin entry points that allow pausing/unpausing the contract
    and changing the admin. The admin is optionally enabled to allow for easier integration
    contracts that may have administration capabilities.
- [shared/fa2_clients](shared/fa2_clients) - FA2 client contracts used for testing.

## Sub-Projects Structure

The sub-projects symlink shared code into their respective directories. Each
sub-project has `ligo` directory that contains all LIGO-related files:

- symlinked shared common code.
- `src` directory with the LIGO implementation of the particular FA2 contract(s).

## Implemented FA2 Contracts

### [English Auction](english_auction)
An implementation of an English auction marketplace that allows users to initiate auctions of NFTs. There are (as of writing this) 
two versions of this contract: one with administration capabilities and one without. For now, in the positive case, this means that
a configured administrator is responsible for configuring and managing access to important entrypoints. 
### [Fixed Price Sale](fixed_price_sale)
An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price. There are multiple flavors of this contract that vary along two variables: admin capabilities, and NFT sale type (fungible tokens/tez).
### [Minter Collection](minter_collection)
Implementations of various token types.
