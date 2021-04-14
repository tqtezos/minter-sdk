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
