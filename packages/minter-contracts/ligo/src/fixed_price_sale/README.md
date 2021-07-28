# Fixed Price Sale

The contracts titled `fixed_price_sale_market.mligo` and
`fixed_price_sale_market_tez.mligo` allow NFT and Fungible Edition Set sales for a fixed price in FA2 tokens or tez respectively. 

If the contract is used for the sale of a Fungible Edition set, the seller will specify a set of N fungible editions (sharing the same FA2 `token id`) and the contract will sell each edition at the same specified price until all are sold out. 

## <a name="storage-section"></a> Storage

The next two subsections will list and explain the storage contents of each
contract. We will refer to the contract
`fixed_price_sale_market.mligo` as `SALE_FA2` and
`fixed_price_sale_market_tez.mligo` as `SALE_TEZ`.

### <a name="sale-fa2-section"></a>`SALE_FA2` Storage

The contract's storage is:

``` ocaml
type storage = 
[@layout:comb]
{
    admin: pauseable_admin_storage;
    sales: (sale_id, sale) big_map;
    next_sale_id : sale_id;
}
```

The first field of this record is `admin`. Its type is:

``` ocaml
type pauseable_admin_storage_record = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}

type pauseable_admin_storage = pauseable_admin_storage_record option
```

The `admin` storage fields are used by the [admin entrypoints](#admin-entrypoints). 

The second field of this record`sales`is a big_map
of currently active sales that the fixed price sale contract is hosting. A sale
is "currently active" after a seller has initiated it up until  the point 
that it is bought by some user for its asking price. The key of this big_map is a unique `sale_id` of type `nat` that identifies a given sale. Storage variable `next_sale_id` is used as the `sale_id` each time a sale is initiated, and then incremented by 1. The value in the `sales` bigmap for a given `sale_id` is some record of type

``` ocaml
type sale =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data;
}
```
where 

``` ocaml
type sale_data =
[@layout:comb]
{
  sale_price: nat;
  sale_token : global_token_id;
  money_token : global_token_id;
  amount : nat;
}
```

and 

``` ocaml
type global_token_id =
  [@layout:comb]
  {
      fa2_address : address;
      token_id : token_id;
  }
```

The record `sale` contains the address of the sale's seller and the sale's
parameters (which has type `sale_data`). The sale's parameters correspond
to the sale_price represented by some `nat` amount of an FA2, the address and token identifier of the NFT for sale (`sale_token`), the address and token identifier of the FA2 token used as the sale's trade currency (`money_token`), and the number of tokens available for the given price (`amount`). 


### <a name="sale-tez-section"></a>`SALE_TEZ` Storage

The contract's storage is:

``` ocaml
type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  sales: (sale_id, sale_tez) big_map;
  next_sale_id : sale_id;
}
```
The storage is almost the same as in the `FA2` version but now we do not have to include an FA2 address and token_id pair to specify the token for which bidding will take place in:

``` ocaml
type sale_tez =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data;
}
```
where 

``` ocaml
type sale_data_tez =
[@layout:comb]
{
  sale_price: nat;
  sale_token : global_token_id;
  amount : nat;
}

## <a name="entrypoints-section"></a> Entrypoints

The entrypoints for both versions of the fixed price sale contract
are named the same, but they accept different parameters.

The entrypoints for [`SALE_FA2`](#sale_fa2-storage) are:

``` ocaml
type market_entry_points =
  | Sell of sale_data
  | Buy of sale_id
  | Cancel of sale_id
  | Admin of pauseable_admin

```

and the entrypoints for [`SALE_TEZ`](#sale_tez-storage) are:

``` ocaml
type market_entry_points =
  | Sell of sale_data_tez
  | Buy of sale_id
  | Cancel of sale_id
  | Admin of pauseable_admin

```

### %sell

If an admin is enabled, the admin can call the `sell` entrypoint in order to sell an NFT/FT Edition set that they own. If a non admin calls the entrypoint and admin capabilites are configured, the call will fail with `NOT_AN_ADMIN,` Otherwise, when admin capabilites are not configured or in the `cancel_only_admin` contract version, any address can call the entrypoint. They specify the token(s) they would like to sell with the `sale_token`. In the FA2 version of the contract, they also specify th FA2 token for which they would like the NFT to be purchased with using `money_token`. 

They also specify the amount of the FA2 they would like to receive in exchange for each token using the `sale_price` field and the number of tokens (which share the same FA2 `token-id`) using `amount`. In this way, the contract can be used to sell a set of fungible tokens which are used to represent an Edition set. Each token is listed for the same price, and they are bought individually. As these tokens share the same `token-id` they are not an NFT Edition set, but rather a set of the same fungible tokens being used to represent an Edition set.  

Before making the call to `sell`, the seller must first add the Fixed price contract as an operator for their NFT, by calling the `Update_operators` entrypoint of the FA2 contract for which the NFT is defined in, and passing the NFT token_id. 

The call to `sell` will fail if the caller is not an `admin` (in the case that admin functionality is enabled), if the fixed price contract was not correctly added as an operator for the NFT, or if the seller does not in fact own the NFT they specify.

If the call to `sell` succeeds, the Fixed price contract will have transferred the NFT to itself and will hold it in escrow until it is either purchased or the seller cancels the sale. It will also keep in storage a record of the sale until the sale is over.

### %buy

A user calls this entrypoint in order to buy an item for sale in the contract. A buyer will specify the token they would like to purchase as well as provide the required funds. In the tez version of the contract, they will attach the required amount in tez to the call to `buy.` In the FA2 version of the contract, they will instead add the Fixed price sale contract as an opperator for the token they would like to place bids in and only then call the `buy` entrypoint. 

The call will fail if the caller does not have the required funds or if they did not correctly add the Fixed price sale contract as an operator. If the contract is paused, the call to `buy` will fail as well. 

If the call to `buy` succeeds, the Fixed price contract will have first initiated a transfer of the funds from buyer to seller. In the Tez version of the contract, this amounts to sending the buying price in tez to the seller. In the FA2 version, the Fixed Price contract will call the FA2 `transfer` entrypoint and transfer the correct number of tokens from buyer to seller. Next, the Fixed price contract will have transferred the token from itself to the buyer by calling the FA2 transfer entrypoint. 

The contract will decrement the `amount` by 1 if the sale was advertising a FT edition set. If `amount == 0` at the end of the purchase (in the case of an NFT sale, or when all the tokens in the FT Edition set have been sold), the contract will also delete the big map record of the sale. If any of these steps failed, the call to `buy` necessarily failed as well.

### %cancel

If the contract is not paused, a seller or an administrator can
cancel an active sale (initiated by the seller). They simply specify the sale they
would like to cancel. Their call will fail if they are not the seller or admin with `NOT_AN_ADMIN_OR_A_SELLER`, or if the sale they specify does not exist with `NO_SALE`. If no admin is configured, only the seller can cancel their active sale. A `cancel` attempt by a non-seller will fail with `NOT_AN_ADMIN_OR_A_SELLER` as well. 

Upon a sale's successful cancellation, the fixed price
sale contract transfers the token(s) back to the seller and
deletes the record of the cancelled sale.

### %admin entrypoints
An `admin` can pause/unpause the contract, guard other operations to be invoked only by the admin and change the admin using two steps confirmation. 

*  `set_admin` - Admin initiate a process that will set the current administrator to
   some specified address.
*  `confirm_admin` - If set_admin entrypoint was successfully run, the `pending_admin` set by the admin in that step calls this entrypoint to confirm themself as admin. 
*  `pause` - pause/unpause the contract. When the contract is paused any entrypoint other than these 3 admin entrypoints will fail with `PAUSED`.

Any call to these entrypoints will fail with `NOT_AN_ADMIN` if admin capabilites are enabled, and will fail with `NO_ADMIN_CAPABILITIES_CONFIGURED` if the contract was not originated with admin capabilites. 

- Ligo Contracts : [FA2 Version](fixed_price_sale_market.mligo), [Tez Version](fixed_price_sale_market_tez.mligo)

<a name="allowlisted-extension"></a>
## Allowlisted extension

Some contract versions allow restricting the set of FA2 contracts that can be used for `Sale_tokens`. Only tokens from specifically defined contracts can be sold in these versions of the contract. 

<a name="allowlist-entrypoints"></a>
### Entrypoints

#### Update allowlist

```ocaml
| Update_allowed of (address, unit) big_map
```

This entrypoint allows setting a new allowlist, overriding the current one.

It accepts `(address, unit) big_map` for the sake of efficiency (allowlist is kept in this form in the storage).

Can be invoked only by the admin, fails with `NOT_ADMIN` otherwise.

### Modification of the base marketplace contract

Each contract with allowlist restriction inherits the behaviour of the respected non-restricted contract.

#### FA2-money version ([`fixed_price_sale_market.mligo`](fixed_price_sale_market.mligo) and its extensions)

Respective allowlisted contract is [`fixed_price_sale_market_allowlisted.mligo`](fixed_price_sale_market_allowlisted.mligo).

* `Sell` entrypoint fails
  + with `SALE_ADDRESS_NOT_ALLOWED` in case any `token_for_sale_address` is not from the allowed list;

Other entrypoints are not modified.

#### Tez version ([`fixed_price_sale_market_tez.mligo`](fixed_price_sale_market_tez.mligo) and its extensions)

Respective allowlisted contract is [`fixed_price_sale_market_tez_allowlisted.mligo`](fixed_price_sale_market_tez_allowlisted.mligo).

* `Sell` entrypoint fails with `SALE_ADDRESS_NOT_ALLOWED` in case any `token_for_sale_address` is not from the allowed list.

Other entrypoints are not modified.

<a name="Fixed-fee-extension"></a>
## Fixed Fee extension

The `fixed_fee` contract versions allow for the configuration of a fixed perecent of any sale to be paid to a fixed address. 

``` ocaml
type fee_data = 
  [@layout:comb]
  {
    fee_address : address;
    fee_percent : nat;
  }
```

`fee_percent` is a nat between 0 and 100 representing the percent of the sale to be paid to the `fee_address`. If `fee_percent > 100`, the `sell` and `buy` entrypoints will both fail with `FEE_TOO_HIGH`.  Fee is subtracted from the buying price and is calculated as `((fee_percent * price) / 100)`. For example, if an NFT is listed for 100tz, and `fee_percent := 30` then 70tz will be sent to the seller and 30tz will be sent to the `fee_address`.

The `fixed_fee` contracts are activated with the `FEE` macro. 

<a name="Per-sale-fee-extension"></a>
## Per Sale Fee Extension

The `per_sale_fee` contract versions are similar to the `fixed_fee` versions but differ in that fee data is configurable per sale as opposed to per contract-- `fee_data` is pased to the `configure` entrypoint in these contracts as opposed to being passed as storage data upon contract origination. 

The `per_sale_fee` contracts are activated by the `PER_SALE_FEE` macro, which is mututally exclusive with the `FEE` macro used to activate the `fixed_fee` contracts. 

<a name="Cancel-only-admin-extension"></a>
## Cancel only admin extension

In the normal fixed price sale with admin enabled, admins have sole authority over configuring and cancelling sales. For some use-cases however it is useful for anyone to have the power to configure a sale, and admins to only have sole authority over cancelling sales. This is accomplished when the C Macro `CANCEL_ONLY_ADMIN` is defined as in the LIGO files with `cancel_only_admin` in the title. 

- Ligo Contracts : [Tez Version](fixed_price_sale_market_tez_cancel_only_admin.mligo), [FA2 Version](fixed_price_sale_market_cancel_only_admin.mligo)

<a name="Offchain-purchase-extension"></a>
## Offchain purchase extension

This extension is useful for sales in which users are not necessarily onboarded to Tezos. They can buy tokens with a credit card, and the contract admin can purchase the item on their behalf without sending Tez/FA2 using a permit similar to the implementation in [One step permit](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-17/tzip-17.md#one-step-permit-entrypoints) calling the `Offchain_buy` entrypoint, potentially submitting multiple permits in batch. 

```
Offchain_buy : (list %offchain_buy
               (pair (nat %sale_id)
                     (pair %permit (key %signerKey) (signature %signature))))
```

Only an admin can call this entrypoint with a permit. If an admin submits a purchase order with a permit, no payment is necessary as it is assumed payment is handled offchain. For these reasons, `Offchain_buy` is not formally a `One-step permit` entrypoint as in [TZIP-17](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-17/tzip-17.md#batched) as its behavior is different from the normal `Buy` entrypoint. 
 
In contrast to the normal `Buy` entrypoint, after the `Offchain_buy` entrypoint is called, the token purchase is marked as pending. When the purchase is pending, no one else can purchase the token. However, the fixed price contract holds the payment (in case of a non-permited purhcase) and token in escrow until the purchase is either approved or denied by the admin. Typically, admin will wait until the offchain payment is either approved or rejected. 

Additionally, the purchaser cannot attempt to buy another token from a given sale again, until the admin either approves or rejects the previous purchase attempt-- otherwise the contract call will fail with `PENDING_PURCHASE_PRESENT`. 

The admin can approve purchases in batch by calling:

`Confirm_purchases : (list %confirm_purchases (pair (nat %sale_id) (address %purchaser)))`

and reject purchases in batch by calling:

`Revoke_purchases : (list %revoke_purchases (pair (nat %sale_id) (address %purchaser)))`.

If a purchase was made using a permit, calling `Confirm_purchases` will send the token to the purchaser and remove the token from sale. `Revoke_purchases` will update storage so that the purchase attempt is deleted and the token is put back up for sale. 

This extension also defines the Macro `OFFCHAIN_MARKET` which makes optional changes to the base fixed price sale contracts to allow for the offchain extension. It changes the storage structure by adding the `pending_purchases` set to the sale record (the value in the `sales` big_map). `pending_purchases` is automatically configured to the empty set upon a sale creation. Furthermore, upon an attempted sale `Cancel`, `pending_purchases` must be empty or the operation will fail with error `PENDING_PURCHASES_PRESENT`, upon which `Revoke_purchases` ought to be called to return the pending purchases. 

```
Pending_purchases : (set %pending_purchases address)
```

### Permit creation

Permit creation is the same as the specification in [TZIP-17](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-17/tzip-17.md#specification-2). Additionally, it should be noted that when permits are submitted in batch, it is necessary to increment the counter that will be used to sign the `BLAKE2B` hash of the packed parameter for each permit in the list. 

- Ligo Contracts : [Tez Version](fixed_price_sale_market_tez_offchain.mligo), [FA2 Version](fixed_price_sale_market_offchain.mligo)


### Tez Stuck Guards
Guards are placed on entrypoints to ensure no tez is sent to them, as to avoid the tez getting stuck in the contract. 
- In the FA2 version of the fixed price contract, no tez can be sent to any entrypoint. An attempt to do so wil fail with `DONT_TRANSFER_TEZ_TO_ANY_ENTRYPOINT`. 
- In the Tez version of the fixed price contract, tez is only expected to be transferred to `Buy`, however there are only guards placed on `Cancel` and `Sell` to minimize costs.
