# Minter Contracts

The `@tqtezos/minter-contracts` package provides a collection of NFT and marketplace smart contracts with configurable admin permissions.

- [Minter Contracts](#minter-contracts)
  - [Provided Contracts](#provided-contracts)
    - [Minter Collection](#minter-collection)
    - [Editions FA2](#editions-fa2)
    - [English Auction](#english-auction)
    - [Fixed Price Sale](#fixed-price-sale)
    - [FA2-FA2 swaps](#fa2-fa2-swaps)
    - [Ticket-based NFTs](#ticket-based-nfts)
    - [Work-in-progress contracts](#work-in-progress-contracts)
      - [Meta-transaction based minting / sales (WIP)](#meta-transaction-based-minting--sales-wip)
      - [Fractional Ownership (WIP)](#fractional-ownership-wip)
      - [Royalties and Profit-splitting (WIP)](#royalties-and-profit-splitting-wip)
- [Local Development](#local-development)
  - [Prerequisites](#prerequisites)
  - [Package Scripts](#package-scripts)
    - [`yarn compile-ligo [filter]`](#yarn-compile-ligo-filter)
    - [`yarn generate-types`](#yarn-generate-types)
    - [`yarn bootstrap`](#yarn-bootstrap)
    - [`yarn bootstrap-sandbox`](#yarn-bootstrap-sandbox)
    - [`yarn bootstrap-testnet`](#yarn-bootstrap-testnet)

## Provided Contracts

### [Minter Collection](ligo/src/minter_collection)

Customizable smart contracts for minting FA2 NFTs as collections.

### [Editions FA2](ligo/src/minter_collection/editions)
The _Editions_ variant of FA2 allows for the minting and distribution of many editions of an NFT that share the same metadata, but with unique identifiers (`token_id`s). The design of this contract also allows for the minting of many editions runs in O(n) (where n is the number of editions runs minted) and concurrent distribution of editions across multiple creators.

### [English Auction](ligo/src/english_auction)

An implementation of an English auction marketplace that allows users to initiate auctions of NFTs in either tez or FA2 tokens.

### [Fixed Price Sale](ligo/src/fixed_price_sale)

An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price in tez or FA2. These contracts can be configured based on a range of administrative options.

### [FA2-FA2 swaps](ligo/src/swaps)

An implementation of a swaps contract that allows two participants to safely exchange their FA2 tokens.
There is a whitelisted extension that allows specifying the permitted set of FA2 contracts involved.

### [Ticket-based NFTs](ligo/src/tickets)

**EXPERIMENTAL** An implementation of NFTs using tickets and a dutch auction example, along with wallet contracts for the NFTs. _Please note: tickets are a new Tezos feature and care should be taken when using them as they have not been heavily tested in production._

### Work-in-progress contracts

#### Meta-transaction based minting / sales ([WIP](https://github.com/tqtezos/minter-sdk/pull/33))

#### Fractional Ownership ([WIP](https://github.com/tqtezos/smart-contracts/pull/57))

#### Royalties and Profit-splitting ([WIP](https://github.com/tqtezos/minter-sdk/pull/40))


# Local Development

## Prerequisites

You'll need these.

- [`docker`](https://www.docker.com/products/docker-desktop)

- [`tezos-client`](https://assets.tqtezos.com/docs/setup/1-tezos-client/)

## Package Scripts

Package scripts are managed and invoked by `yarn`.


### `yarn compile-ligo [filter]`

Compile LIGO contracts to Michelson. 

Accepts an optional filter which compiles _only_ those contracts matching the given filter string. 

E.g., 
```bash
yarn compile-ligo fa2_swap
```

If no filter is given, all contracts will be compiled.

**Arguments**

| Argument   | Position    | Description                                                | Required |
| --------   | ----------- | ---------------------------------------------------------  | -------- | 
| `[filter]` | `0`         | Compile _only_ contracts matcing the given filter string   | 🔘        |


**Options**

| Option | Alias               | Description              | Required |
| ------ | ------------------- | -----------------------  | -------- | 
| `-s`   |`--src-path`         | LIGO source path         | 🔘       |
| `-o`   |`--out-path`         | Michelson output path    | 🔘       |

One may also pass the `help` command to see a list of options in their terminal.
```bash
yarn compile-ligo help
```

> This script delegates LIGO compilation to `docker` — ensure the docker daemon is running for it to execute correctly.


### `yarn generate-types`

This will generate the contract types and code files in `bin-ts`

```bash
yarn generate-types
```

> This script will _not_ compile LIGO contracts beforehand. Be sure to execute [`yarn compile-ligo`](#yarn-compile-ligo-filter) first if you need updated contract code.


### `yarn bootstrap` 
Bootstrap the network specified in `ENV_NAME` environment name.
Check if the contract addresses in the config file are actually deployed on
the network. If necessary, re-deploy compiled contracts and update the config
file.

### `yarn bootstrap-sandbox` 
Bootstrap flextesa sandbox network.

### `yarn bootstrap-testnet`
Bootstrap flextesa test network.
