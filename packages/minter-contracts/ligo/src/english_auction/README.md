# English Auction

This contract implements a typical English auction contract, with bids to be made in tez. It has a few distinguishing features

1. It handles the auctioning off of multiple assets in a single contract.
2. An asset can include a collection of FA2 tokens (possibly across different contracts) and amounts.
3. The contract can be configured with an admin, who has the sole power to configure auctions for the contract, as well the power to transfer admin privelages to another address.

## Storage
The structure of the storage is as follows:

```sh=
#The strictly increasing id of the next asset to be auctioned.
nat %current_id;

#Max time in seconds before an auction is considered ended.
nat %max_auction_time;

#Upper bound on how long between configuration and start time for an auction. Ensures fake auctions don't fill up big_map
nat %max_config_to_start_time;

bigmap %auctions nat %asset_id {
  seller : address; # SENDER that configured auction
  current_bid : mutez; #Upon configuration, set as opening_price
  start_time : timestamp; #When bidding can begin
  assets : list ({fa2_address : address; token_id : nat; qty : nat});
  min_raise_percent : nat; #percentage increase of the previous bid used to determine the minimum valid subsequent bid.
  min_raise : tez; #the amount increase of previous bid in tez used to determine the minimum valid subsequent bid.
  end_time : timestamp;
  extend_time : int; #The amount by which to extend the auction from NOW if bid placed at NOW is within extend_time from end_time.
  highest_bidder : address; #Upon config, set as SENDER. Afterwards, set to be previous bidder.
  last_bid_time : timestamp;
  round_time : nat; #In seconds-- the amount of time from when the last bid was placed for a bidder to place a new bid.
}

#The admin API can change an admin address using two step confirmation pattern.
#Admin can also pause/unpause the contract.
#This functionality is optional.
pauseable_admin_storage %pauseable_admin

```
## Auction State

- An auction is considered to be `ended` if either the current time is greater than or equal to the end time of the auction or `round_time` has passed after the last bid was placed. 

- An auction is considered to have `started` if the current time is greater than or equal to the auction's start time.

- An auction is `in_progress` if it has `started` and not `ended`. 

## Bid validity

We determine whether a given bid is of a sufficiently high value as follows: 

The bid is greater than the the previous bid plus the ceiling of the `min_raise_percent` of it. 

OR

The bid is greater than the previous bid plus the `min_raise`

OR 
                                  
The bid is greater than or equal to the opening_price and it is the first bid to be placed. 

## Entrypoints

### %configure
An auction can be configured with the parameters specified in `configure_param` if the following conditions are met: 

1. Admin is set and `SENDER` is an admin, or if no admin is set.
2. Both `min_raise` AND `min_raise_percent` are non-zero in order to avoid the possibility of empty raises.
3. The `AMOUNT` sent to the `configure` entrypoint is 0mutez.
4. `end_time` is after `start_time`.
5. `start_time` is greater than or equal to `NOW`.
6. `round_time` is greater than 0 seconds. 
7. `opening_price` is greater than 0mutez/fa2. 
8. `Fee_percent` is less than or equal to 100% (where percent is a natural number). 

When the conditions are met, 
 
`auctions[current_id]` is set with parameter values, `current_bid` is set to `opening_price` and `storage.current_id` is incremented. 

The contract optimistically transfers assets from `SENDER` to itself. That means `SENDER` needed to already have approved the transfer to the auction contract of the assets that they are auctioning. The auction configuration fails if any of these transfers fail.

```bash=
%configure   {
    opening_price : tez;
    min_raise_percent : nat;
    min_raise : tez;
    round_time : nat;
    extend_time : nat;
    asset : {fa2_address : address; token_id : nat; qty : nat} list;
    start_time : timestamp;
    end_time : timestamp;
  }

```

### %bid
A new bid can be placed if the following conditions are met: 

1. The auction is in progress 
2. `AMOUNT` is a valid bid amount.
3. The sender is neither the `seller` nor the last `highest_bidder`.
4. The sender is an implicit account. 

When these conditions are met, a call to this entrypoint returns previous bid to the last bidder and updates auction storage variables accordingly. If bid is placed within `extend_time` from `end_time`, extend the auction by `extend_time` from bid time (time returned by Michelson `NOW` at time of bid) to prevent front running wars at end. 

```sh=
%bid {
  asset_id : nat;
}
```

### %cancel
If `SENDER` is `seller` or `admin` and auction is in progress, a call to this entrypoint will return `assets` to `owner`and return `current_bid` to `highest_bidder`. It will also delete auction data from assets big_map. `cancel` must be called with an `AMOUNT` equal to 0 tez. 

```sh=
  %cancel {
    asset_id : nat;
  }
```

### %resolve
If an auction has ended, a call to this entrypoint ought to send the asset to `highest_bidder` and `current_bid` to owner. It also delete auction data from assets big_map. `resolve` must be called with an `AMOUNT` equal to 0 tez. 

```sh=
%resolve {
  asset_id : nat;
}

```
## Errors 

- `AUCTION_DOES_NOT_EXIST`: Auction does not exist for given `asset_id`
- `INVALID_END_TIME`: `end_time` must be after `start_time`
- `INVALID_AUCTION_TIME`: `end_time - start_time` must be less than or equal to `max_auction_time`
- `INVALID_START_TIME`: `start_time` must not have already passed
- `MAX_CONFIG_TO_START_TIME_VIOLATED`: `start_time` must not be greater than the sum of current time and `max_config_to_start_time`
- `INVALID_OPENING_PRICE`: `Opening_price` must be greater than 0mutez
- `DONT_TRANSFER_TEZ_TO_{entrypoint}: `AMOUNT` sent to {entrypoint} must be 0mutez
- `INVALID_ROUND_TIME`: `Round_time` must be greater than 0 seconds
- `INVALID_RAISE_CONFIGURATION`: Both `min_raise_percent` AND `min_raise` must be non-zero.
- `INVALID_FEE`: `Fee_percent` must be less than or equal to 100%. Please originate another contract.
- `AUCTION_ENDED`: Auction cannot be cancelled if it has ended. 
- `AUCTION_NOT_ENDED`: Auction cannot be resolved if it has NOT ended. 
- `BIDDER_NOT_IMPLICIT`: Bidder must be an implicit account
- `NOT_IN_PROGRESS`: An auction must be in progress in order to place a new bid. 
- `SEllER_CANT_BID`: Seller cannot place a bid on their own item.
- `NO_SELF_OUTBIDS`: A bidder cannot outbid themself if they were the previous highest bidder.
- `INVALID_BID_AMOUNT`, (current_bid, Tezos.amount, highest_bidder, last_bid_time, Tezos.now))`: A bid of `Tezos.amount` was placed at `Tezos.now` which was invalid. At the time the bid was placed the highest bid is `current_bid` placed at `last_bid_time` by `highest_bidder`.
- `ASSET_NOT_ALLOWED`: Only allowlisted NFTs can be listed for sale. 
- `NOT_AN_ADMIN`: Only an admin can interact with this entrypoint. 
- `NOT_AN_ADMIN_OR_A_SELLER`: Only an admin or the seller can cancel a configured auction. 

## Allowlisted extension

Some contract versions allow restricting the set of FA2 contracts that can participate in them.

We provide several implementations of allowlist as described in the
[allowlist modules documentation](../../fa2_modules/README.md#fa2-allowlist-modules).

`Update_allowed` entrypoint can be invoked only by the admin, failing with `NOT_ADMIN` otherwise.

### Modification of the base marketplace contract

Each contract with allowlist restriction inherits the behaviour of the respected non-restricted contract.

Besides, the following restriction takes place:
* For `%configure` entrypoint and its variations: in case any of `fa2_address`/`token_id` in the assets list does not belong to the allowlist, `ASSET_NOT_ALLOWED` error is raised.

## Design Choices/Cautions

1. Only Implicit accounts can place bids.
2. **CAUTION:**   Smart contracts can configure bids. Bidders are encouraged to inspect the code of the selling contract and confirm that the first bid as well as the final reward (upon a call to `Resolve`) will be accepted and not be used to steal gas in the process. See https://www.notion.so/Review-report-for-English-auction-contract-iteration-2-c3610435cc1446d1b6f2b2d60dc86c8e for more details on this.
3. An auction can be configured with an empty asset list theoretically, although this would be a fruitless auction. This was decided to be as such in order to minimuze the cost of configuring an auction. 
4. In the tez auction contracts, "tez guards" are only placed on `cancel`, `configure`, and `resolve` to minimize costs. However, only bid is expected to actually receive tez. Admins should not transfer tez to any other entrypoints to avoid it getting stuck in the contract. 

## Future work

1. Full front running protection.

# English Auction w/ FA2 bids

In this version of the NFT English Auction contract, bids are made in some FA2 token specified at contract origination as `bid_currency`. Bidders must add the auction contract as an operator for their bid token and then specify the amount they would like to bid in the set FA2 token (`bid_currency`) as an argument to the `bid` entrypoint. The entrypoint will fail if the auction cannot transfer the indicated amount of `bid_currency` to itself from the bidder. In this version of the contract, no tez can be transferred to any entrypoint. An attempt to do so will fail with `DONT_TRANSFER_TEZ_TO_ANY_ENTRYPOINT`. 

# English Auction with permit configuration

This is an implementation of the auction contract in which the standard `Configure` entrypoint is replaced by one that accepts a batch of optional permits that can be used to configure auctions for accounts different than `SENDER.` This could serve useful for applications looking to allow users without tez to auction off their assets. See [One-step Permit](https://gitlab.com/tzip/tzip/-/merge_requests/151) for reference.

# English Auction with Auction Fee

This is a version of the Auction contract in which an address fixed at contract origination gets a fixed percent of any sale that takes place using the contract. The fee is calculated using normal integer division, which rounds down. Therefore, it is possible even with a positive fee percentage that the fee sent will be 0 if the final price of the item being auctioned off is too low. 

# Cancel only admin extension

In the normal english auction contract with admin enabled, admins have sole authority over configuring and cancelling auctions. For some use-cases however it is useful for anyone to have the power to configure an auction, and admins to only have sole authority over cancelling auctions. This is accomplished when the C Macro `CANCEL_ONLY_ADMIN` is defined as in the LIGO files with `cancel_only_admin` in the title.
