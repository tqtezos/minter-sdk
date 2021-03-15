![Minter SDK header](/docs/mintersdkhead.png)

[![](https://img.shields.io/badge/license-MIT-brightgreen)](LICENSE)

## Notice

This software is in beta. At the moment, the smart contracts
that OpenMinter uses have **not** been formally audited. Please
use this software at your own risk.

## Minter SDK

Minter SDK offers a set of tools and libraries to bring creation, management, and sales of NFTs to any application. 

Minter SDK contains:
- A collection of NFT-related smart contracts with configurable admin permissions
- Typescript bindings to enable easy integration into popular web frameworks
- An NPM module that allows users to interact with tokens programmatically
- A CLI that brings SDK functions to the command line

## Smart Contracts

### [Minter Collection](minter_collection)
Implementations of various token types.

### [English Auction](english_auction)
An implementation of an English auction marketplace that allows users to initiate auctions of NFTs. There are (as of writing this) 
two versions of this contract: one with administration capabilities and one without. For now, in the positive case, this means that
a configured administrator is responsible for configuring and managing access to important entrypoints. 
### [Fixed Price Sale](fixed_price_sale)
An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price. There are multiple flavors of this contract that vary along two variables: admin capabilities, and NFT sale type (fungible tokens/tez).

## Work-in-progress

### Edition contracts

### FA2-FA2 swaps

### Fractional Ownership

### Royalties and Profit-splitting
