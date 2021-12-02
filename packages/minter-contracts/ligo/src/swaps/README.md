# Swaps contract

Swaps contract allows one participant (a seller) to offer an exchange with FA2 tokens, that may be accepted by another participant.

Table of contents:
* [Build instructions](#build-instructions)
* [Basic swap contract](#basic-swap-contract)
  * [Types](#swap-basic-types)
  * [Entrypoints](#swap-basic-entrypoints)
  * [Notes to the middleware implementors](#notes-to-the-middleware-implementors)
* [Allowlisted extension](#allowlisted-extension)
  * [Contract parts](#contract-parts)
  * [Types](#allowlist-types)
  * [Entrypoints](#allowlist-entrypoints)

## Build instructions

To compile the contracts, run `yarn compile-ligo`.

## Basic swap contract

Source code: [ligo](fa2_swap.mligo).

This contract may contain a multitude of swaps, each of them follows the life-cycle presented in the diagram:

![lifecycle](./swap-lifecycle.png)

<a name="swap-basic-types"></a>
### Types

The following types are expressed in cameligo.

#### Basic types

A `swap_id` type represents identifier assigned to a single exchange event.

```ocaml
type swap_id = nat
```

A `fa2_tokens` type represents several FA2 tokens of specific type.

```ocaml
type fa2_tokens =
  [@layout:comb]
  { token_id : nat
  ; amount : nat
  }
```

A `fa2_assets` type represents a list of tokens within one FA2 contract.

```ocaml
type fa2_assets =
  [@layout:comb]
  { fa2_address : address
  ; tokens : fa2_tokens list
  }
```

Note that we expect that provided list of tokens will be grouped by FA2 contracts the tokens belong to. Despite the more complicated interface, this lets us require fewer `get_entrypoint_opt` calls and make use of FA2 batched transfers and thus be much more efficient.

A `swap_offers` type contains all the data provided at swap creation.

```ocaml
type swap_offers = 
  [@layout:comb]
  {
    swap_offer : swap_offer;
    remaining_offers : nat;
  }  

type swap_offer =
  [@layout:comb]
  { assets_offered : fa2_assets list
  ; assets_requested : fa2_assets list
  }

```

A `swap_info` type represents all the full information stored about a single swap.

```ocaml
type swap_info =
  [@layout:comb]
  { swap_offers : swap_offers
  ; seller : address
  }
```

#### Storage

Entries in storage include:
* `%swaps : (swap_id ,swap_info) big_map` with all the swaps.

<a name="swap-basic-entrypoints"></a>
### Entrypoints

Notes:
* By *transfer* (in the context of FA2 tokens) we mean "performing a zero-xtz transaction that invokes FA2 `transfer` entrypoint".
* Entrypoints that perform FA2 transfer may throw the respective errors enlisted in [FA2 interface](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#error-handling).

#### %start

```ocaml
| Start of swap_offer
```

Proposes an exchange.

This transfers the `assets_offered` tokens from the `sender` to our contract (thus assuming that our contract has been made an operator of the sender's tokens).

The swap will have `Open` state, and its `seller` will be initialized to the current `sender`.

The contract automatically assigns a `swap_id` (basing on an auto-incrementing counter) to the swap. We assume that it is the caller's responsibility to extract and record the `swap_id`, it will be available in the applied operation.

In case some of `assets_offered.fa2_address` refer to an invalid FA2 contract (not originated or without `transfer` entrypoint), `SWAP_OFFERED_FA2_INVALID` is raised.

In case any tokens transfer from `sender` to our contract fails (due to lack of tokens or other issues), the respective FA2 error is propagated.

`assets_requested` is not validated and must be checked by the client for a better UX.

We do not check whether any assets list is empty on purpose.

#### %cancel

```ocaml
| Cancel of swap_id
```

Marks the swap as canceled and returns the tokens to the `seller`. Further operations with this swap won't be possible.

Can be called only by the `seller`, otherwise `NOT_SWAP_SELLER` error is raised.

The swap gets removed from big_map, any further operation for this swap will raise `SWAP_NOT_EXIST`.

#### %accept

```ocaml
| Accept of swap_id
```

This
1. Transfers the offered assets from our contract to the `sender`;
2. Transfers the requested assets from the `sender` to the `seller`.

If there are remaining swap offers left (`remaining_offers` >= 1), contract storage will be updated to reflect 1 less remaining offer (i.e. decrement `remaining_offers`). Otherwise, the swap gets removed from big_map, any further operation for this swap will raise `SWAP_NOT_EXIST`.

This can be called by *any* address.


In case some of `assets_requested.fa2_address` refer to an invalid FA2 contract (not originated or without `transfer` entrypoint), `SWAP_REQUESTED_FA2_INVALID` is raised. This error normally should not happen, as the client is supposed to validate `assets_requested` on swap start.

In case any tokens transfer mentioned above fails (due to lack of tokens or other issues), the respective FA2 error is propagated.

### Notes to the middleware implementors

Any client working with this contract has to care about supplying the proper input.

#### Grouping by FA2 address

Assets accepted by `%start` entrypoint has to be grouped by FA2 token address.
In case this is not done, the performance (and fees) may be far from optimal.

#### Validating `assets_requested`

Upon executing `%start` entrypoint, the client has to check `assets_requested`:
  * That the referred FA2 token addresses exist in the chain;
  * That contract with those addresses contain `%transfer` entrypoint;
  * That referred `token_id`s are supported by the FA2 (can be checked in TZIP-16 metadata).

Checking `assets_offered` this way is not strictly necessary as the contract will raise an error in case offered assets are not correct.

#### Errors handling

Be sure to handle not only the errors listed in the entrypoints description, but also errors inherent to FA2 interface.

### Design choices

1. Forbid empty offered/requested assets to avoid user's mistakes.
2. Preserve info about resolved swaps in storage - decided not to go with this is favor of using blockchain explorers.

## Allowlisted extension

This contract additionally provides a way to restrict the set of allowed FA2 tokens that can participate in exchanges.
This set is managed by the administrator entity.

Source code: [ligo](fa2_allowlisted_swap.mligo).

### Contract parts

The plan is to develop one contract consisting of a few components that can be reused for other purposes.

The contract has to consist of the following parts:
1. Base swap contract;
2. Simple administrator contract. We picked the existing [`simple_admin` contract](../../fa2_modules/admin/non_pausable_simple_admin.mligo) that is already present in the `minter-sdk` repository.
3. Allowlisting component (reuses the swaps contract and implements the allowlisting capabilities).

The last point will be described further in this document.

<a name="allowlist-types"></a>
### Types

The storage of the allowlist component is solely `big_map address ()`.

We do not use mere `list` or `set` because `big_map` is more efficient for the `Start` entrypoint.

<a name="allowlist-entrypoints"></a>
### Entrypoints

#### Update allowlist

```ocaml
| Update_allowed of (address, unit) big_map
```

This entrypoint allows setting a new allowlist, overriding the current one.

It accepts `(address, unit) big_map` for the sake of efficiency (allowlist is kept in this form in the storage).

Can be invoked only by the admin, fails with `NOT_ADMIN` otherwise.

### Modification of the base swaps contract

The allowlisting component provides a method for modifying behaviour of `main` of the base swaps contract. This ensures that the allowlisting capability can be composed with other extensions to the swaps contract that may be requested in the future.

#### Start swap entrypoint

In case any of `assets_offered.fa2_address` or `assets_requested.fa2_address` addresses do not appear in the allowlist `big_map`, `SWAP_OFFERED_FA2_NOT_ALLOWLISTED` or `SWAP_REQUESTED_FA2_NOT_ALLOWLISTED` error is raised respectively.

#### Cancel swap entrypoint

Not updated.

#### Accept swap entrypoint

Not updated.

### Contract Extension Macros

#### XTZ_FEE

When the `XTZ_FEE` macro is activated, the `assets_requested` data in `swap_offers` is changed to a tuple, with the second element in the tuple encoding some amount in tez that must be included along with the requested set of tokens when the swap is accepted. Like the requested tokens, the fee sent in tez is sent to the user who proposed the swap. If the correct amount is not sent to the contract, it will fail with `SWAP_REQUESTED_XTZ_INVALID`.

```ocaml
type swap_offer =
  [@layout:comb]
  { assets_offered : fa2_assets list
  ; assets_requested : fa2_assets list * tez
  }
```

#### BURN_PAYMENT

When `BURN_PAYMENT` macro is activated, the contract storage is modified to include a `burn_address`. Furthermore, upon acceptance of a swap, all FA2 tokens in `assets_requested` will be sent to the `burn_address` by the contract instead of being sent to the seller. This macro is useful in versions of the contract in which the buyer is expected to "pay" for a swap with some tokens that are to be destroyed at the conclusion of the transaction. Note, When `XTZ_FEE` is activated at the same time, the tez fee is NOT sent to the `burn_address` but to the seller as expected. 

```ocaml
type swap_storage =
  { next_swap_id : swap_id
  ; swaps : (swap_id, swap_info) big_map
#if BURN_PAYMENT
  ; burn_address : address
#endif
  }
```

#### OFFCHAIN_SWAP

This macro defines a new entrypoint `Offchain_accept` to which a contract admin can send a list of `permit_accept_param`s where 

```ocaml
type permit_accept_param = 
  [@layout:comb]
  {
    swap_id : nat;
    permit : permit;
  }
```
and

```ocaml
type permit = 
  [@layout:comb]
  {
    signerKey: key;
    signature: signature;
  }

``` 

in order to accept a given `swap_id` for a user offchain. 

When `XTZ_FEE` is activated at the same time, it is no longer necessary for the admin to send the requested `tez` to the contract as it is assumed payment is handled off chain. When `BURN_PAYMENT` is activated, the FA2 tokens will be burned to the `burn_adddress` as expected. 

### Alternative Contract Versions

#### Offchain Swap with Burn and Fee

- [ligo](fa2_swap_offchain_burn_fee.mligo)
- [Michelson](../../../bin/fa2_swap_offchain_burn_fee.tz)

This contract activates `XTZ_FEE`, `OFFCHAIN_SWAP`, and `BURN_PAYMENT`. It is intended to be compiled with the main function as `allowlisted_swaps_offchain_main`, which adds the new `Offchain_accept` entrypoint to the modified base swap contract with allowlist extension as well as burn and xtz-fee features. 

```
{ parameter
    (or (or %baseSwap
           (or (or %admin (unit %confirm_admin) (address %set_admin))
               (or %swap
                  (or (nat %accept) (nat %cancel))
                  (pair %start
                     (pair %swap_offer
                        (list %assets_offered
                           (pair (address %fa2_address) (list %tokens (pair (nat %token_id) (nat %amount)))))
                        (pair %assets_requested
                           (list (pair (address %fa2_address) (list %tokens (pair (nat %token_id) (nat %amount)))))
                           mutez))
                     (nat %remaining_offers))))
           (big_map %update_allowed address unit))
        (list %offchain_accept
           (pair (nat %swap_id) (pair %permit (key %signerKey) (signature %signature))))) ;
  storage
    (pair (pair (pair %admin (address %admin) (option %pending_admin address))
                (big_map %allowlist address unit))
          (pair %swap
             (pair (address %burn_address) (nat %next_swap_id))
             (big_map %swaps
                nat
                (pair (pair %swap_offers
                         (pair %swap_offer
                            (list %assets_offered
                               (pair (address %fa2_address) (list %tokens (pair (nat %token_id) (nat %amount)))))
                            (pair %assets_requested
                               (list (pair (address %fa2_address) (list %tokens (pair (nat %token_id) (nat %amount)))))
                               mutez))
                         (nat %remaining_offers))
                      (address %seller))))) ;
```

#### Swap with Collections and Burn (and Offchain Accept)

- [ligo](fa2_swap_with_collections_and_burn.mligo) 
- [Offchain ligo](fa2_swap_with_collections_and_burn_offchain.mligo) 
- [Michelson](../../../bin/fa2_swap_with_collections_and_burn.tz)
- [Offchain Michelson](../../../bin/fa2_swap_with_collections_and_burn_offchain.tz)

This contract adds a notion of `collections` to the base swap contract.  A `collection` is simply a set of `token_id`s from a given FA2.  The contract stores a list of collections (more precisely a `Big_map` mapping a `collection_id` to a `collection`). An admin can also add new collections to the contract through
 the `Add_collection` entrypoint. Now, the swap proposer  must specify a list of `collection_id`s in the `assets_requested` field as opposed to a list of `token_id`s. Accordingly, the swap accepter can accept the swap by sending a _unique_ token from each of the `collections` specified in `assets_requested`. This contract also activates the `Offchain_accept` and `Burn` features which behave similarly to above with the exception that the user must sign the entire `accept_param` provided to the `Accept` entrypoint, which now includes both the 
`swap_id` as well as the list of tokens `tokens_sent` the swap accepter would like to use in accepting the swap. 

The new entrypoint structure of the Collections contract without `Offchain_accept` is as follows: 

```ocaml
type swap_entrypoints =
  | Start of swap_offers
  | Cancel of swap_id
  | Accept of accept_param
  | Add_collection of collection_info
  | Admin of admin_entrypoints

where 

type collection_id = nat

type swap_id = nat

type fa2_token =
  [@layout:comb]
  { token_id : token_id
  ; amount : nat
  }

type tokens = fa2_token list 

type tokens_sent = (collection_id * token_id) set (*Set data structure enforces uniqueness of tokens sent to satisfy a collection*)

type accept_param = 
  [@layout:comb]
  {  swap_id : swap_id 
  ;  tokens : tokens_sent
  } 

type collection_info = token_id set (*Set data structure enforces uniqueness of tokens in a given set*)

type swap_offer =
  [@layout:comb]
  { assets_offered : tokens
  ; assets_requested : collection_id list (*Swap offerrer is expected to sort set_ids in ascending order*)
  }

type swap_offers = 
  [@layout:comb]
  {
    swap_offer : swap_offer;
    remaining_offers : nat;
  }  
```
and the remaining types are the same as the base swap contract. When the `Collections` contract is extended in the `Offchain` version, a new `Offchain_accept` entrypoint is added as follows: 

```ocaml
type offchain_swap_entry_points =
  | BaseSwap of swap_entrypoints
  | Offchain_accept of permit_accept_param list
``` 
and the contrct is intended to be compiled with `swaps_offchain_main` as the main function.

A few things to make note of about the entrypoints. 

 1. The `collection_id`s in `assets_requested` must be sorted in ascending order. For example, if the swap proposer would like to receive 4 tokens from collection 1 and 2 tokens from collection 4, they would submit the `assets_requested` list as `[1, 1, 1, 1, 4, 4]`. The operation may still succeed if they do not sort the list, but behavior of the swap will be unpredictable. 

 2. `tokens_sent` is a `set` of the tuple `(collection_id * token_id)`. For each entry in `tokens_sent` the accepter specifies the `token_id` they would like to send to satisfy the request for a `collection_id` listed in `assets_requested`. The `set` data structure enforces uniqueness so a consequence is that a user cannot use the same token multiple times to satisfy a collection. Another consequence of this is that if the swap proposer requests more tokens from a given collection than the size of that collection, the swap will be unsatisfiable. 

3. As the type of `collection_info` is also a `set` of `token_id`s, a `collection` can only contain unique `token_id`s. 

The new storage is as follows:

```ocaml
type swap_storage =
  { next_swap_id : swap_id
  ; next_collection_id : collection_id
  ; swaps : (swap_id, swap_info) big_map
  ; burn_address : address
  ; collections : (collection_id, collection_info) big_map
  ; fa2_address : address (*Every token this contract interacts with must be defined in this FA2*)
  ; admin : admin_storage
  }
```
A key point to make note of is that `fa2_address` is a storage variable and is used in contract logic. As such, `assets_offered` and `collection_info` for example do not contain any data about the FA2 contract of the tokens they reference, because it is assumed that every token referenced is defined in the FA2 at `fa2_address`. In this version of the contract then, it is not possible to swap FA2 tokens from multiple FA2 contracts.