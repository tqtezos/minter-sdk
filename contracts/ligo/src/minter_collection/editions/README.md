# Editions variant of FA2

## Intro

We want to mint many _editions_ of an NFT that share the same metadata,
but with unique serial numbers, e.g. in a _limited run_.
With contracts not-specialized to editions, this results in redundant copies of the metadata that's shared between editions in a run.
We also want the token Ids of tokens in an edition set to be consecutive, to make reasoning about them easier. 

This propsal allows editions creators to easily mint an "unlimited" (limited only by big_map storage limits) number of editions and then distribute them concurrently. 

## Storage

- FA2 + TZIP-16 storage

- Editions-specific storage
  + `current_token_id : nat`
  + `current_edition_id : nat`
  + `editions_metadata`

```ocaml
editions_metadata :=
  big_map
    (nat : edition_id)
    (token_metadata
    , address : creator
    , initial_token_id : nat
    , nat : number_of_editions
    , nat : number_of_editions_to_distribute)
```
## Entrypoints

- `mint_editions : list mint_edition`
  + Only admin can call this entrypoint if admin is configured
  + `mint_edition := (token_metadata, nat : number_of_editions)`
  + For each entry in the list, an entry is created in `editions_metadata`for key `current_edition_id`
    with:
    * `number_of_editions = number_of_editions_to_distribute`
    * `initial_token_id = current_token_id`
    * `current_token_id += number_of_editions`
    * `creator = SENDER`
    * `current_edition_id ` incremented by 1

- `distribute_editions : list (edition_id, to_ : nat * address)`
  + For each pair in the list
    * A single `initial_token_id + (number_of_editions - number_of_editions_to_distribute)` token is minted to the `to_` `address`
    * `number_of_editions_to_distribute` is decremented (fail if `== 0`)
    * `current_token_id` is incremented by `1`
