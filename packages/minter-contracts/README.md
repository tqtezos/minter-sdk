# Minter Contracts

The `@tqtezos/minter-contracts` package provides a collection of NFT and marketplace smart contracts with configurable admin permissions.

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

### Work-in-progress contracts

#### Edition contracts ([WIP](https://github.com/tqtezos/minter-sdk/pull/46))

#### Meta-transaction based minting / sales ([WIP](https://github.com/tqtezos/minter-sdk/pull/33))

#### Fractional Ownership ([WIP](https://github.com/tqtezos/smart-contracts/pull/57))

#### Royalties and Profit-splitting ([WIP](https://github.com/tqtezos/minter-sdk/pull/40))

---

## Package Scripts

Package scripts are managed and invoked by `yarn`.

1. `yarn compile-contracts` — compile all LIGO contracts with the currently installed LIGO compiler.

2. `yarn bootstrap` — bootstrap the network specified in `ENV_NAME` environment name.
   Check if the contract addresses in the config file are actually deployed on
   the network. If necessary, re-deploy compiled contracts and update the config
   file.

3. `yarn bootstrap-sandbox` — bootstrap flextesa sandbox network.

4. `yarn bootstrap-testnet` — bootstrap flextesa test network.
