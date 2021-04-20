# Fixed Price Sale

The contracts titled `fixed_price_sale_market.mligo` and
`fixed_price_sale_market_tez.mligo` both contain implementations of a sale of a
non-fungible token (NFT) at a fixed price. The former contract allows a seller
to sell an NFT for some fixed price of FA2 tokens. The latter instead allows a
seller to sell an NFT for some fixed price of tez. 

## <a name="storage-section"></a> Storage

The next two subsections will list and explain the storage contents of each
contract. We will refer to the contract
`fixed_price_sale_market.mligo` as `SALE_FA2` and
`fixed_price_sale_market_tez.mligo` as `SALE_TEZ`.

### <a name="sale-fa2-section"></a>`SALE_FA2` Storage

The contract's storage is:

``` ocaml
type storage = {
    admin: pauseable_admin_storage;
    sales: (sale_param, nat) big_map;
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
that it is bought by some user for its asking price. The key of this big_map is a record
type uniquely specifying an active sale (excluding its sale price). Its definition
is:

``` ocaml
type sale_tokens_param =
[@layout:comb]
{
 token_for_sale_address: address;
 token_for_sale_token_id: token_id;
 money_token_address: address;
 money_token_token_id: token_id;
}

type sale_param =
[@layout:comb]
{
  seller: address;
  tokens: sale_tokens_param;
}
```

The record `sale_param` contains the address of the sale's seller and the sale's
parameters (which has type `sale_token_param`). The sale's parameters correspond
to the address and token identifier of the NFT for sale and the address and
token identifier of the FA2 token used as the sale's trade currency. The
corresponding fields are respectively, `token_for_sale_address`,
`token_for_sale_token_id`, `money_token_address` and `money_token_token_id`.

The value of each entry in the big_map (`sales`) is the number of FA2 tokens that the seller
requests for the NFT. 

### <a name="sale-tez-section"></a>`SALE_TEZ` Storage

The contract's storage is:

``` ocaml
type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  sales: (sale_param_tez, tez) big_map;
}
```
Like before, we keep a "record" of currently active sales. Unlike before, the
value of each entry in the big_map is an NFT's price, but specified in tez
instead of FA2 tokens. The key (type = `sale_param_tez`) is the sale's parameters, but now we do not have to include an FA2 address and Token_id pair to specify the token for which bidding will take place in:

``` ocaml
type sale_token_param_tez =
[@layout:comb]
{
 token_for_sale_address: address;
 token_for_sale_token_id: token_id;
}

type sale_param_tez =
[@layout:comb]
{
  seller: address;
  sale_token: sale_token_param_tez;
}

```

## <a name="entrypoints-section"></a> Entrypoints

The entrypoints for both versions of the fixed price sale contract
are named the same, but they accept different parameters.

The entrypoints for [`SALE_FA2`](#sale_fa2-storage) are:

``` ocaml
type market_entry_points =
  | Sell of init_sale_param
  | Buy of sale_param
  | Cancel of sale_param
  | Admin of pauseable_admin

```

and the entrypoints for [`SALE_TEZ`](#sale_tez-storage) are:

``` ocaml
type market_entry_points =
  | Sell of init_sale_param_tez
  | Buy of sale_param_tez
  | Cancel of sale_param_tez
  | Admin of pauseable_admin

```

### %sell

An admin can call the `sell` entrypoint in order to sell an NFT that they own. They specify the NFT they would like to sell with the fields `token_for_sale_address` and `token_for_sale_token_id.` In the FA2 version of the contract, they also specify th FA2 token for which they would like the NFT to be purchased with using the fields `money_token_address` and `money_token_token_id`. They also specify the amount of the FA2 they would like to receive in exchange for their NFT using the `sale_price` field. 

``` ocaml
type init_sale_param =
[@layout:comb]
{
  sale_price: nat;
  sale_tokens_param: sale_tokens_param;
}
```

Before making the call to `sell`, the seller must first add the Fixed price contract as an operator for their NFT, by calling the `Update_operators` entrypoint of the FA2 contract for which the NFT is defined in, and passing the NFT token_id. 

The call to `sell` will fail if the caller is not an `admin`, if the Fixed price contract was not correctly added as an operator for the NFT, or if the seller does not in fact own the NFT they specify.

If the call to `sell` succeeds, the Fixed price contract will have transferred the NFT to itself and will hold it in escrow until it is either purchased or the seller cancels the sale. It will also keep in storage a record of the sale until the sale is over.

### %buy

A user calls this entrypoint in order to buy an item for sale in the contract. A buyer will specify the NFT they would like to purchase as well as provide the required funds. In the tez version of the contract, they will attach the required amount in tez to the call to `buy.` In the FA2 version of the contract, they will instead add the Fixed price sale contract as an opperator for the token they would like to place bids in and only then call the `buy` entrypoint. 

The call will fail if the caller does not have the required funds or if they did not correctly add the Fixed price sale contract as an operator. If the contract is paused, the call to `buy` will fail as well. 

If the call to `buy` succeeds, the Fixed price contract will have first initiated a transfer of the funds from buyer to seller. In the Tez version of the contract, this amounts to sending the buying price in tez to the seller. In the FA2 version, the Fixed Price contract will call the FA2 `transfer` entrypoint and transfer the correct number of tokens from buyer to seller. Next, the Fixed price contract will have transferred the NFT from itself to the buyer by calling the FA2 transfer entrypoint. It will also delete the big map record of the sale. If any of these steps failed, the call to `buy` necessarily failed as well.

### %cancel

If the contract is not paused, a seller or an administrator can
cancel an active sale (initiated by the seller). They simply specify the sale they
would like to cancel. Their call will fail if they are not the seller or admin, 
if the sale they specify does not exist. 

Upon a sale's successful cancellation, the fixed price
sale contract transfers the NFT back to the seller and
deletes the record of the cancelled sale.

### %admin entrypoints
An `admin` can pause/unpause the contract, guard other operations to be invoked only by the admin and change the admin using two steps confirmation. 

*  `set_admin` - Admin initiate a process that will set the current administrator to
   some specified address.
*  `confirm_admin` - If set_admin entrypoint was successfully run, the `pending_admin` set by the admin in that step calls this entrypoint to confirm themself as admin. 
*  `pause` - pause/unpause the contract. When the contract is paused any entrypoint other than these 3 admin entrypoints will fail with `PAUSED`.

<a name="allowlisted-extension"></a>
## Allowlisted extension

Some contract versions allow restricting the set of FA2 contracts that can participate in them.

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

#### FA2-money version ([`fixed_price_sale_market.mligo`](fixed_price_sale_market.mligo))

Respective allowlisted contract is [`fixed_price_sale_market_allowlisted.mligo`](fixed_price_sale_market_allowlisted.mligo).

* `Sell` entrypoint fails
  + with `SALE_ADDRESS_NOT_ALLOWED` in case any `token_for_sale_address` is not from the allowed list;
  + with `MONEY_ADDRESS_NOT_ALLOWED` in case any `money_token_address` is not from the allowed list;

Other entrypoints are not modified.

#### Tez version ([`fixed_price_sale_market_tez.mligo`](fixed_price_sale_market_tez.mligo))

Respective allowlisted contract is [`fixed_price_sale_market_tez_allowlisted.mligo`](fixed_price_sale_market_tez_allowlisted.mligo).

* `Sell` entrypoint fails with `SALE_ADDRESS_NOT_ALLOWED` in case any `token_for_sale_address` is not from the allowed list.

Other entrypoints are not modified.
