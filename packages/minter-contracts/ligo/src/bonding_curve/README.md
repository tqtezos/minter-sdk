# Bonding Curve Contract for Non-Fungible Tokens

The bonding curve contract interfaces with any NFT marketplace contract that
supports minting and burning tokens, allowing users to buy and sell tokens
indefinitely without creating new auctions.

## Bonding Curve Storage

- `admin : admin_storage`
  + Simple admin storage

- `market_contract : address`:
  + FA2 contract supporting `Mint` and `Burn`
  + I.e. "marketplace" contract

- `auction_price : tez`:
  + Final price of the auction

- `token_index : nat`:
  + Number of tokens sold. This number must be positive to sell back tokens.
  + I.e. `token_index` must be `20` to sell back up to `20` tokens after an auction where users bought at least `20` tokens
  + You may want to add a constant piecewise polynomial segment at the beginning with `tokens_sold_in_auction` length
    and value `token_final_cost_in_auction`

- `token_metadata : token_metadata`:
  + Token metadata for minting
  + When `Buy` or `Buy_offchain` are called, this `token_metadata` is used to
    mint a NFT on the `market_contract` (with a unique token id)

- `basis_points : nat`:
  + The percentage (in basis points) cost of buying and selling a token at the same index
  + In other words, the fee in basis points for using this contract

- `cost_mutez : piecewise_polynomial`:
  + The bonding curve formula, as a piecewise polynomial
  + See a definition and explanation of the `piecewise_polynomial` type in `Appendix A`
    cost_mutez : piecewise_polynomial;

- `unclaimed : tez`:
  + Any tez that's unclaimed as a result of the `basis_points` fee


## Bonding Curve Entrypoints

- Simple Admin entrypoints, i.e. `update_admin`, etc.

- `Set_deletgate`
  + Parameter: `key_hash option`
  + Spec:
    * Admin-only
    * Set the delegate of the contract to the given `key_hash` if present, or unset if `None`

- `Withdraw`
  + Parameter: `unit`
  + Spec:
    * Admin-only
    * The amount of tez in `unclaimed` (in storage) is sent to the admin

- `Buy`
  + Parameter: `unit`
  + Spec:
    * Requires the bonding curve contract to be a minter
    * Requires tez sent equal to the price
    * Price is calculated as the sum of
      - `auction_price`
      - `cost_mutez` applied to `token_index`
      - `(auction_price + cost_mutez) * (basis_points / 10,000)` (integer division)
    * Mints token using `token_metadata` from storage to buyer
    * Increments `token_index`
    * Adds the `basis_points` fee to the `unclaimed` tez in storage

- `Buy_offchain`
  + Parameter: `address`
  + Spec:
    * Anyone can call it (equivalent to calling buy as admin and then transferring to the offchain_address)
    * Has all requirements of the `Buy` entrypoint
    * `address` is the buyer's address, the minted NFT is sent here
    * This entrypoint is the same as `Buy`, except the minted token is sent to
      the buyer's address

- `Sell`
  + Parameter:
  + Spec:
    * Required the bonding curve contract to be a minter
    * The sender must be the owner of the token to sell
    * `token_id` is token to sell
    * Price is calculared as in `Buy`, without the `basis_points` fee, i.e. as the sum of:
      - `auction_price`
      - `cost_mutez` applied to `token_index`
    * The token is burned on the FA2 marketplace
    * Tez equal to the price is sent to the seller
    * The `token_index` is decremented

- `Sell_offchain:`
  + Parameter: `token_id * address`
  + Spec:
    * Admin-only
    * Has all requirements of the `Sell` entrypoint, except can be called by admin without being a token owner
    * `token_id` is token to sell
    * `address` is the sellers's address, the NFT is burned from this account and the tez are sent here
    * This entrypoint is the same as `Sell`, except the token is burned from the
      given seller's address and the tez is sent to that seller's address


## NFT Contract

Updated NFT (marketplace) contract on which NFT's are minted/traded

Storage: no storage type updates, but an update to the semantics:
The token with `token_id = 0` must be held by the admin of the marketplace to
for any minting or burning and any address which is an operator of `token_id = 0`
for the admin address is allowed to mint or burn tokens. Such a user is called a
"minter".

Entrypoints:
- `Update_metadata`
  + Parameter: `token_metadata list`
  + Spec:
    * Admin-only
    * The given `token_metadata`'s are inserted into the
      `token_metadata : big_map token_id token_metadata` `big_map`,
      updating any currently-present `token_id`'s metadata.
  + Misc: this entrypoint can't be used to delete token metadata

- `Burn`:
  + Parameter: `token_id * (bytes * address)`
  + Spec:
    * Minter-only
    * `bytes` is the `symbol` of the NFT to burn
    * `address` is the owner of the NFT to burn
    * The token is deleted from the ledger and `token_metadata` `big_map`


## Appendix A: Piecewise Polynomial's

### Polynomials: Coefficient Lists

The Mathematica function [CoefficientList](https://reference.wolfram.com/language/ref/CoefficientList.html)
is implemented equivalently.

In short, the following polynomial:

```
f(x) = a0 * x^0 + a1 * x^1 + .. + an * x^n
```

Is represented as the list:

```
[a0, a1, .. , an]
```

Where the coefficient of `x^i` is the `ith` element of the list.

This is exactly the definition of the `polynomial` type in ligo:

```
type polynomial =
  [@layout:comb]
  {
    coefficients : int list;
  }
```

Note that coefficients are `int`'s: floating point numbers are not supported in
Michelson, but their behavior may be simulated to arbitrary precision.


### Piecewise Polynomials

Given our representation of polynomials, because we're only concerned with
inputs over the natural numbers, we can represent a piecewise polynomial in the
following way:

First, we represent a single finite segment as a pair of a natural number length
and a polynomial:

```
(length_0, polynomial_0) => polynomial_0(x) | 0 < x < length_0
```

And glue two or more segments together using their length's

```
(length_0, polynomial_0) => polynomial_0(x) | 0                   <= x < length_0
(length_1, polynomial_1) => polynomial_1(x) | length_0            <= x < length_0 + length_1
(length_2, polynomial_2) => polynomial_2(x) | length_0 + length_1 <= x < length_0 + length_1 + length_2
..
```

Finally, we can account for the infinite remaining segment with a single
polynomial.

In other words, when `x >= length_0 + length_1 + .. + length_last`, we apply
a polynomial with no segment length:

```
(length_0, polynomial_0) => polynomial_0(x) | 0                              <= x < length_0
(length_1, polynomial_1) => polynomial_1(x) | length_0                       <= x < length_0 + length_1
(length_2, polynomial_2) => polynomial_2(x) | length_0 + length_1            <= x < length_0 + length_1 + length_2
(length_2, last_segment) => last_segment(x) | length_0 + length_1 + length_2 <= x
..
```

Here's it in one place:

```ocaml
// A segment of a piecewise function
type piecewise_segment = 
  {
    length : piecewise_length;
    poly : polynomial;
  }

type piecewise_polynomial =
  {
    segments : piecewise_segment list;
    last_segment : polynomial;
  }
```

