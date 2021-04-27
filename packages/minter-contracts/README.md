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
- [JavaScript / TypeScript Package](#javascript--typescript-package)
  - [Package Installation](#package-installation)
  - [Architecture](#architecture)
    - [Contract Code (for origination)](#contract-code-for-origination)
      - [Contract import type shape](#contract-import-type-shape)
    - [Contract Signatures](#contract-signatures)
    - [Network Client](#network-client)
      - [Example usage with `taquito`](#example-usage-with-taquito)
- [Local Development (Contributing)](#local-development-contributing)
  - [Prerequisites](#prerequisites)
  - [Package Scripts](#package-scripts)
    - [`yarn compile-ligo [filter]`](#yarn-compile-ligo-filter)
    - [`yarn generate-types`](#yarn-generate-types)
    - [`yarn bootstrap`](#yarn-bootstrap)
    - [`yarn bootstrap-sandbox`](#yarn-bootstrap-sandbox)
    - [`yarn bootstrap-testnet`](#yarn-bootstrap-testnet)

## Provided Contracts

### [Minter Collection](/packages/minter-contracts/ligo/src/minter_collection)

Customizable smart contracts for minting FA2 NFTs as collections.

### [Editions FA2](/packages/minter-contracts/ligo/src/minter_collection/editions)
The _Editions_ variant of FA2 allows for the minting and distribution of many editions of an NFT that share the same metadata, but with unique identifiers (`token_id`s). The design of this contract also allows for the minting of many editions runs in O(n) (where n is the number of editions runs minted) and concurrent distribution of editions across multiple creators.

### [English Auction](/packages/minter-contracts/ligo/src/english_auction)

An implementation of an English auction marketplace that allows users to initiate auctions of NFTs in either tez or FA2 tokens.

### [Fixed Price Sale](/packages/minter-contracts/ligo/src/fixed_price_sale)

An implementation of an NFT marketplace that allows users to initiate NFT sales at a fixed price in tez or FA2. These contracts can be configured based on a range of administrative options.

### [FA2-FA2 swaps](/packages/minter-contracts/ligo/src/swaps)

An implementation of a swaps contract that allows two participants to safely exchange their FA2 tokens.
There is an extension with allowlist that allows specifying the permitted set of FA2 contracts involved.

### [Ticket-based NFTs](/packages/minter-contracts/ligo/src/tickets)

**EXPERIMENTAL** An implementation of NFTs using tickets and a dutch auction example, along with wallet contracts for the NFTs. _Please note: tickets are a new Tezos feature and care should be taken when using them as they have not been heavily tested in production._

### Work-in-progress contracts

#### Meta-transaction based minting / sales ([WIP](https://github.com/tqtezos/minter-sdk/pull/33))

#### Fractional Ownership ([WIP](https://github.com/tqtezos/smart-contracts/pull/57))

#### Royalties and Profit-splitting ([WIP](https://github.com/tqtezos/minter-sdk/pull/40))

# JavaScript / TypeScript Package

The contracts are packaged into TypeScript modules and published to npm for use in JavaScript / TypeScript projects.

## Package Installation
with `npm`
```
npm i @tqtezos/minter-contracts
```

with `yarn`
```
yarn add @tqtezos/minter-contracts
```

## Architecture

### Contract Code (for origination)

The code for each contract is exported directly from the `@tqtezos/minter-contracts` package. 

The naming convention used for contract code exports is:
```typescript
${TitleCasedContractName}Code
```
Where `{TitleCasedContractName}` denotes an interpolation of the title-cased name of any particular contract.

For example, to access the code for the [`fa2_swap`](/packages/minter-contracts/ligo/src/swaps) contract, one would import:
```typescript
import { Fa2SwapCode } from '@tqtezos/minter-contracts';
``` 

#### Contract import type shape
Each exported contract code object is wrapped in a special unique type to make it easy to inspect at runtime or with the type system, if necessary. 

This type has the following shape:

```typescript
export declare const ${TitleCasedContractName}Code: {
  __type: ${TitleCasedContractName};
  protocol: string;
  code: object[];
};
```

> Note, this isn't valid TypeScript.

Inspecting the type of the imported `Fa2SwapCode` object from above, it has the following shape:
```typescript
export declare const Fa2SwapCode: {
  __type: 'Fa2SwapCode';
  protocol: string;
  code: object[];
};
```

The raw Michelson code is available at the `.code` property.

```typescript
import { Fa2SwapCode } from '@tqtezos/minter-contracts';

// Log the contract's Michelson code
console.log(Fa2SwapCode.code);
```

### Contract Signatures
This package exports additional types representing the signature of any particular contract.

The naming convention for these types is similar to the code import.

```typescript
${TitleCasedContractName}ContractType
```

Again, our [`fa2_swap`](/packages/minter-contracts/ligo/src/swaps) contract example:

```typescript
import type { Fa2SwapContractType } from '@tqtezos/minter-contracts'
```

This type will provide type information about the methods and storage on the contract:

```typescript
export declare type Fa2SwapContractType = {
  methods: Methods;
  storage: Storage;
  code: {
    __type: 'Fa2SwapCode';
    protocol: string;
    code: object[];
  };
};
```

Where `Methods` and `Storage` represent the unique shape of the contract's entrypoints and storage, respectively.

### Network Client

This particular package _solely_ exports smart contract code – it doesn't assume any particular Tezos client. As such, one will need to pick their favorite JavaScript / TypeScript Tezos client to actually interact with the contracts on the Tezos network. We recommend the [`@taquito/taquito`](https://tezostaquito.io/) package.

#### Example usage with `taquito`

```typescript
import { Fa2SwapCode } from '@tqtezos/minter-contracts';
import { TezosToolkit } from '@taquito/taquito';

const tezos = new TezosToolkit('https://YOUR_PREFERRED_RPC_URL');
tezos.contract.originate({
  code: Fa2SwapCode.code,
  // ...
});
```

See the [`@taquito/taquito` package's documentation](https://tezostaquito.io/docs/quick_start) for more information on interacting with the `taquito` package. 

# Local Development (Contributing)

## Prerequisites

You'll need these.

- [`docker`](https://www.docker.com/products/docker-desktop)
- [`yarn`](https://yarnpkg.com/getting-started/install)
  
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
