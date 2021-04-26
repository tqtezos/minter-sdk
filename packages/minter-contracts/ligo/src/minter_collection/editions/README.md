# Editions variant of FA2

## Intro

We want to mint many _editions_ of an NFT that share the same metadata,
but with unique serial numbers, e.g. in a _limited run_.
With contracts not specialized to editions, this results in redundant copies of the metadata that's shared between editions in a run.
We also want the token ids of tokens in an edition set to be consecutive to make reasoning about them easier. 

Editions-FA2 allows editions creators to easily mint an "unlimited" (limited only by big_map storage limits) number of editions and then distribute them concurrently. 

## Storage

- FA2 w/ single-admin + TZIP-16 storage without `Token_metadata` big_map or `next_token_id`
  + [Michelson](../../../../bin/fa2_multi_nft_asset.tz)
  + [Ligo](../nft/fa2_multi_nft_asset_simple_admin.mligo)

- Editions-specific storage
  + `editions_metadata : editions_metadata`
  + `max_editions_per_run : nat`

```ocaml
editions_metadata :=
  big_map
    (nat : edition_id)
    ( edition_info : ((string, bytes) map)
    , creator : address 
    , number_of_editions : nat
    , number_of_editions_to_distribute : nat)
```

## Entities

**Edition Creator** - A tezos account that has created an `Edition run` of size N by calling `mint_edition`.

**Edition run/Edition set** - A set of N `token_id`s that when minted to, will share the same `token_metadata`. A given `Edition run` will be represented by a unique `edition_id` and is created upon a call to `mint_editions.`

**Edition** - An NFT with a `token_id`, belonging to some `Edition run`. An edition is minted to Alice upon a call to `distribute_editions` that includes Alice's address in the distribution list for that edition's `edition_id`. 

## Entrypoints

- `mint_editions : list mint_edition_run`
  + **GUARDS USED:** `fail_if_not_admin` 
  + `mint_edition_run := (edition_info', number_of_editions' : ((string, bytes) map) * nat )`
  + For each entry in the list, an entry is created in `editions_metadata`for key `next_edition_id`
    with:
    * `number_of_editions = number_of_editions_to_distribute`
    * `creator = SENDER` (admin)
    * `edition_info = edition_info'`

  + Additionally, `next_edition_id ` is incremented by `1`.

- `distribute_editions : list (edition_id, receivers : nat * (address list))`
  + **GUARDS USED:** `fail_if_paused` 
  + For each `Edition run` corresponding to a given `edition_id`, `editions` are distributed to the addresses in `receivers`. Each distribution mints a new token to a `token_id` equal to `edition_id` * `max_editions_per_collection` + `(number_of_editions - number_of_editions_to_distribute`)
  + Only a creator of some edition run can distribute for their `Edition run`.
  + A creator cannot distribute more than the `number_of_editions` set in the creation of the `Edition run`.
  + A creator can distribute editions for multiple edition runs that they created, in a single call to `distribute_editions`. 

  For each "distribution":
    * `mint_edition` reserves `max_editions_per_run` `token_id`s beginning at `next_edition_id * max_editions_per_collection` , and `distribute_edition` uses them sequentially.
    * We fail if more editions are distributed than were initially allocated by `mint_editions`

## Errors 
- If a non-admin attempts to call `Mint_editions`, `Set_admin`, or `Pause` fail with `NOT_AN_ADMIN`.
- If the contract is paused and a user attempts to call any FA2 entrypoint or `Distribute_editions`,failwith `PAUSED`. 
- If a user attempts to mint an `Edition_run` larger than `max_editions_per_run` fail with `EDITION_RUN_TOO_LARGE`. 
- If a user attempts to distribute more editions than were created, the call fails with error `NO_EDITIONS_TO_DISTRIBUTE`. 
- If a user attempts to distribute from an `Edition run` that has not been created, the call fails with error `INVALID_EDITION_ID`.
- If a user attempts to distribute from an `Edition run` for which they are not the `creator` the call fails with error `INVALID_DISTRIBUTOR`.

## Offchain-View
Given a valid `token-id` the offchain view will return the edition metadata of the `Edition run` that edition belongs to.

## FA2 
+ Once an edition is `distributed` it can be transferred just as any NFT is, by its owner calling the `transfer` entrypoint.

## Admin 
+ The contract has a single admin at any given time, that can be updated following the two-step procedure described in [Simple-Admin](../../../fa2_modules/README.md). The admin can pause the contract and has sole authority over minting traditional NFTs as well as creating an `Edition run` through `Mint_editions`. If admin is changed, the new admin cannot distribute editions from an `Edition run` that they did not create. 

## Extending this contract
+ Note, it is not possible when using this contract as a module to implement a custom transfer permission policy using [Transfer hooks](../../../fa2/fa2_hook.mligo). As such, it is necessary for `OWNER_HOOKS` to be undefined in order to compile this contract as well as any contract that extends it. 

## Performance 
Although the contract has not been heavily testsed, brief benchmarking suggests that it is possible to mint upwards of 10 trillion copies of some edition set (as we are only limited by constraints on the `nat` type in Michelson) and then distribute ~394 copies of some edition in a single call to `distribute_edition`. 

See https://better-call.dev/edo2net/KT1S6CShPP79LWrHyLxkJ8prL7hRuQ8u6193/operations for an example of an originated version of https://github.com/tqtezos/minter-sdk/blob/950c32fa02c32a27c0e11849671b1135656b4c07/packages/minter-contracts/bin/fa2_multi_nft_token_editions.tz on edonet. 

In that contract a call to `mint_editions` for a 100 edition `Edition set` cost ~0.045tz and a call to `distribute_editions` for the distribution of 20 editions cost ~0.46tz. 
