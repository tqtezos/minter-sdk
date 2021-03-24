![Minter SDK header](/docs/mintersdkhead.png)

[![](https://img.shields.io/badge/license-MIT-brightgreen)](LICENSE)

## Notice

This software is in beta. At the moment, the smart contracts
that OpenMinter uses have **not** been formally audited. Please
use this software at your own risk.

## Minter SDK

Minter SDK offers a set of tools and libraries to bring creation, management, and sales of NFTs to any application.

-   A collection of NFT and marketplace smart contracts with configurable admin permissions
-   Typescript bindings to enable easy integration into popular web frameworks
-   An NPM module that allows users to interact with tokens programmatically
-   A CLI for minting and configuring NFT / marketplace contracts
-   [DIDkit](https://www.spruceid.com/didkit) integration for verified creator credentials

## Smart Contracts

### [Minter Collection](contracts/ligo/src/minter_collection)

Customizable smart contracts for minting FA2 NFTs as collections.

### [English Auction](contracts/ligo/src/english_auction)

An implementation of an English auction marketplace that allows users to initiate auctions of NFTs in either tez or FA2 tokens.

### [Fixed Price Sale](contracts/ligo/src/fixed_price_sale)

An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price in tez or FA2. These contracts can be configured based on a range of administrative options.

### [FA2-FA2 swaps](contracts/ligo/src/swaps)

An implementation of a swaps contract that allows two participants to safely exchange their FA2 tokens.

### [Ticket-based NFTs](contracts/ligo/src/tickets)  

**EXPERIMENTAL** An implementation of NFTs using tickets and a dutch auction example, along with wallet contracts for the NFTs. _Please note: tickets are a new Tezos feature and care should be taken when using them as they have not been heavily tested in production._

### [Editions FA2](minter_collection/editions)
The _Editions_ variant of FA2 allows for the minting and distribution of many editions of an NFT that share the same metadata, but with unique identifiers (`token_id`s). This results in a reduction in redundant copies of the same metadata, and thus cheaper contract interaction. The design of this contract also allows for the minting of many editions runs in O(n) (where n is the number of editions runs minted) and concurrent distribution of editions across multiple creators. 
### Work-in-progress contracts

#### Meta-transaction based minting / sales ([WIP](https://github.com/tqtezos/minter-sdk/pull/33))

#### Fractional Ownership ([WIP](https://github.com/tqtezos/smart-contracts/pull/57))

#### Royalties and Profit-splitting ([WIP](https://github.com/tqtezos/minter-sdk/pull/40))
