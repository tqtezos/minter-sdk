# Fixed Price Sale

The contracts titled `fixed_price_sale_market.mligo` and
`fixed_price_sale_market_tez.mligo` both contain implementations of a sale of a
non-fungible token (NFT) at a fixed price. The former contract allows a seller
to sell an NFT for some fixed price of FA2 tokens. The latter instead allows a
seller to sell an NFT for some fixed price of tez. Although, the general
design and implementation of this contract is fairly straightforward, someone
unfamiliar with the Tezos blockchain's inner workings needs to be aware of
certain key items. These details will discussed in the following section
[Entrypoints](#entrypoints-section). For now, we shall discuss the storage
contents of each contract.

## <a name="storage-section"></a> Storage

The next two subsections will list and explain the storage contents of each
contract. For the sake of concision, we hereby label the contract
`fixed_price_sale_market.mligo` as `SALE_FA2` and the other
(`fixed_price_sale_market_tez.mligo`) as `SALE_TEZ`.

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

A thorough explication of this type's semantics will delayed for later in this
document. For now, simply note that this type endows this contract with
administrative operations. Namely, these are the abilities to pause operations
and prevent entrypoint invocation by unauthorized callers (if desired).

The second field of this record`sales`is simply a "record" (actually a big_map)
of currently active sales that the fixed price sale contract is hosting. To be
clear, when we use the phrase "currently active sale", we mean to say that a
seller has initiated some sale and there has of yet been no action taken that
would prevent someone from buying the sale's offered product. The value of each
entry in the big_map (`sales`) is the number of FA2 tokens that the seller
specifies as the NFT's value. The key of this big_map is another record
uniquely specifying an active sale (excluding its sale price). It is definition
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
token identifier of the FA2 token used as the sale's trade currency; the
corresponding fields are respectively, `token_for_sale_address`,
`token_for_sale_token_id`, `money_token_address` and `money_token_token_id`.

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

Please refer to the previous section for an explanation of the `admin` field. It
is precisely the same here. The second field is also similar, but we shall
describe thoroughly as well.

Like before, we keep a "record" of currently active sales. Unlike before, the
value of each entry in the big_map is an NFT's price, but specified in tez
instead of FA2 tokens. The key (type = `sale_param_tez`) is:

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

Again, this key contains a sale's parameters. Importantly, we first see the
address of the sale's seller. Afterwards, we also see some additional sale
parameters in the record `sale_token` (type = `sale_token_param_tez`). This
record is simpler to understand than its counterpart above. It only contains the
address and token id of the NFT on sale. This is unsurprising since a potential
buyer must pay for the NFT on sale with tez!

## <a name="entrypoints-section"></a> Entrypoints

Fortunately, the entrypoints for both versions of the fixed price sale contract
are exactly the same. For ease of exposition, I will highlight the necessary
differences between each pair of entrypoints where necessary. The entrypoints
for [`SALE_FA2`](#sale-fa2-section) are

``` ocaml
type market_entry_points =
  | Sell of init_sale_param
  | Buy of sale_param
  | Cancel of sale_param
  | Admin of pauseable_admin

```

and the entrypoints for [`SALE_TEZ`](#sale-tez-section) are:

``` ocaml
type market_entry_points =
  | Sell of init_sale_param_tez
  | Buy of sale_param_tez
  | Cancel of sale_param_tez
  | Admin of pauseable_admin

```

The records `pauseable_admin`, `sale_param_tez` and `sale_param` have the same
definition as indicated earlier in this document. The Sell section below
contains the definitions of the `init_sale_param` and `init_sale_param_tez`
records.

### %sell

While the purpose of this entrypoint should be readily obvious, it still is
necessary to point out some important details whilst using it. Firstly, if there
exists an administrator of the given fixed price sale contract, that
administrator can pause the contract. If such an occurrence happens, then a
potential seller cannot initiate a sale. This entrypoint invocation will fail
with a message satisfying the FA2 specification.

Provided an un-paused fixed price sale contract, this entrypoint's invocation
will initiate a transfer of the seller's NFT token to the marketplace. It is
CRITICAL that the seller add the marketplace contract as another operator of
their NFT token before initiating the sale. Find some accompanying tests here
(they contain instructive examples):

* [nft_market.test.ts](../../../test/nft_market.test.ts) lines 91, 136
* [nft_market_fa2.test.ts](../../../test/nft_market_fa2.test.ts) lines 143, 146,
  192, 195
* [nft_market_fa2_with_admin.test.ts](../../../test/nft_market_fa2_with_admin.test.ts)
  lines 164, 172, 257
* [nft_market_with_admin.test.ts](../../../test/nft_market_with_admin.test.ts)
  lines 97, 176.

The definition of the function, `addOperator`, is here:

* [fa2_interface.ts](../../../src/fa2-interface.ts) line 46.


These above steps outline apply irrespective of which version of the fixed price
sale contract is in use. The primary difference is the record type of the
variant associated with this entrypoint. For [`SALE_FA2`](#sale-fa2-section) we
have the record type `init_sale_param` we have:


``` ocaml
type init_sale_param =
[@layout:comb]
{
  sale_price: nat;
  sale_tokens_param: sale_tokens_param;
}
```

This record contains the sale price of the NFT in positive integer units
corresponding to the FA2 token used to price the NFT along with
`sale_tokens_param` field described earlier in this document (the section
[Storage](#storage-section)). We have something similar for
[`SALE_TEZ`](#sale-tez-section):

``` ocaml
type init_sale_param_tez =
[@layout:comb]
{
  sale_price: tez;
  sale_token_param_tez: sale_token_param_tez;
}
```

This record, like the one above, contains the sale price in tez (instead of some
FA2 token) and the `sale_token_param_tez`, again described earlier in this
document (the section [Storage](#torage-section)).

### %buy

Like in the previous section, an administrator can prevent this entrypoint's
invocation by pausing the contract. Nonetheless, under normal operation, this
entrypoint will allow a potential buyer to claim the NFT for sale provided they
have funds. If they do not have the funds, they will not lay claim to the NFT
(it will remain under the ownership of the seller). If someone else has bought
the NFT or if the seller has cancelled the sale, the entrypoint will emit an
error message of "NO_SALE".

The record associated to the `Buy` variant is either `sale_param` or
`sale_param_tez`. Earlier sections of this document (see
[Storage](#storage-section)) contain their definitions.

More specifically, let's go through the steps that would transpire when the
`%buy` entrypoint is invoked. As stated shortly before, the `Buy` variant either
contains the `sale_param` or `sale_param_tez` record depending upon the version
of the fixed price sale contract. Regardless of which, here are the basic steps
that should occur following this entrypoint's invocation.

1. Determine if the sale specified in record found in the `Buy` variant is a
   currently active sale. This amounts to a big_map lookup. If this lookup
   retrieval is successful, continue to the following steps while keeping track
   of the price attached to the sale's contents, otherwise emit the
   `failwith` operation with the following message "NO SALE".
2. Create a set of operations corresponding to the transfer of the sale price
   (recorded in the last step) from the buyer's available funds. Of course, this
   will fail if the required funds are not available in the buyer's "purse". To
   further clarify, when buying NFTs with FA2 tokens, this set of operations
   includes two operations. Namely, the first operation sends FA2 tokens to the
   marketplace contract and the second finally transfers the tokens just
   begotten to the sale's seller. To be clear, this is a safe because if any of
   the operations fail, then all will fail.

   The same set of inner steps occurs for the other fixed price sale contract
   (where NFTs are sold in tez), except that the buyer is expected to include
   the required funds in the payload of their `%buy` entrypoint invocation. This
   is the tez that the seller receives.

3. The second to last step transfers the NFT from the marketplace to the buyer.
   Hence, the seller will no longer own the NFT and the marketplace will cease
   to be one of its operators.

4. Finally, the marketplace contract removes the sale associated to the newly
   bought NFT from the record of active sales and emits the list of operations
   corresponding to the events described in the previous steps.

### %cancel

Again as in previous sections, an administrator pausing the contract can prevent
the invocation of this entrypoint. Otherwise, a seller or an administrator can
cancel an active sale (the seller must the sender of associated with this
entrypoint's invocation). Upon a sale's successful cancellation, the fixed price
sale contract transfers the NFT back to its owner and the fixed price sale
deletes the record of the cancelled sale.

The record associated to the `Buy` variant is either `sale_param` or
`sale_param_tez`. Earlier sections of this document (see
[Storage](#storage-section)) contain their definitions.

### %admin
This entrypoint is a gateway to another set of entrypoints for endowing some
contract with administrative capabilities. We deem it worthwhile to point the
interested reader to [README.md](../../fa2_modules/README.md) for a thorough
discussion of each entrypoint. However, for ease of use, here's a summary of
each entrypoint:

*  set_admin - initiate a process that will set the current administrator to
   some specified address. This step doesn't actually set the administrator.
*  confirm_admin - if set_admin entrypoint was successfully run, actually set
   the administrator to the address specified in that necessarily previous step.
*  pause - pause the contract.

With `set_admin` and `confirm_admin` entrypoints, we can change the
administrator of the fixed price sale contract.
