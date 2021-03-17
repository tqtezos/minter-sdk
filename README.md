![Minter SDK header](/docs/mintersdkhead.png)

[![](https://img.shields.io/badge/license-MIT-brightgreen)](LICENSE)

## Notice

This software is in beta. At the moment, the smart contracts
that OpenMinter uses have **not** been formally audited. Please
use this software at your own risk.

## Minter SDK

Minter SDK offers a set of tools and libraries to bring creation, management, and sales of NFTs to any application.

Minter SDK aims to include the following

-   A collection of NFT and marketplace smart contracts with configurable admin permissions
-   Typescript bindings to enable easy integration into popular web frameworks
-   An NPM module that allows users to interact with tokens programmatically
-   A CLI for minting and configuring NFT / marketplace contracts
-   [DIDkit](https://www.spruceid.com/didkit) integration for verified creator credentials

## Smart Contracts

### [Minter Collection](minter_collection)

Customizable smart contracts for minting FA2 NFTs as collections.

### [English Auction](english_auction)

An implementation of an English auction marketplace that allows users to initiate auctions of NFTs in either tez or FA2. There are
two versions of this contract: one with administration capabilities and one without. For now, in the positive case, this means that
a configured administrator is responsible for configuring and managing access to important entrypoints.

### [Fixed Price Sale](fixed_price_sale)

An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price. There are multiple flavors of this contract that vary along two variables: admin capabilities, and NFT sale type (fungible tokens/tez).

### [Minter Collection](minter_collection)

Implementations of various token types.

## [Ticket NFTs](tickets)

**EXPERIMENTAL** An implementation of NFTs using tickets and a dutch auction example, along with wallet contracts for the NFTs. _Please note: tickets are a new Tezos feature and care should be taken when using them as they have not been heavily tested in production._
An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price in either FA2 or tez. There are multiple flavors of this contract that vary along two variables: admin capabilities, and NFT sale type (fungible tokens/tez).

## Work-in-progress

### Edition contracts

### Meta-transaction based minting / sales

### FA2-FA2 swaps

### Fractional Ownership

### Royalties and Profit-splitting
