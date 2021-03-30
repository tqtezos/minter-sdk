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
  extend_time : int; #The amount by which to extend the auction if auction is within extend_time from end_time.
  highest_bidder : address; #Upon config, set as SENDER. Afterwards, set to be previous bidder. 
  last_bid_time : timestamp;
  round_time : nat; #In seconds-- the amount of time from when the last bid was placed for a bidder to place a new bid. 
}

#The admin API can change an admin address using two step confirmation pattern.
#Admin can also pause/unpause the contract. 
#This functionality is optional. 
pauseable_admin_storage %pauseable_admin 

```
These storage variables are used to determine the state of the auction as follows: 

```ocaml=
let auction_ended (auction : auction) : bool =
  ((Tezos.now >= auction.end_time) || (* auction has passed auction time*)
   (Tezos.now > auction.last_bid_time + auction.round_time)) (*round time has passed after bid has been placed*)

let auction_started (auction : auction) : bool =
  Tezos.now >= auction.start_time

let auction_in_progress (auction : auction) : bool =
  auction_started(auction) && (not auction_ended(auction))
  
```

And we determine whether a given bid is valid as follows: 

```ocaml=
let valid_bid_amount (auction : auction) : bool =
  (Tezos.amount >= (auction.current_bid + (ceil_div (auction.min_raise_percent *  auction.current_bid, 100n)))) ||
  (Tezos.amount >= auction.current_bid + auction.min_raise)                                            ||
  ((Tezos.amount >= auction.current_bid) && first_bid(auction))

```
Either `current_bid`  needs to be raised by `min_raise_percent * current_bid` OR `current_bid`  needs to be raised by `min_raise`. If it is the first bid, only the `opening_price` must be met. 

## Entrypoints

### %configure

If admin is set and `SENDER` is an admin, or if no admin is set, `auctions[current_id]` is set with parameter values, `current_bid` is set to `opening_price` and `storage.current_id` is incremented. The `AMOUNT` sent to the entrypoint must be 0mutez additionally.  

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
If auction is in progress and `AMOUNT` is a valid bid amount, a call to this entrypoint returns previous bid or seller's deposit if first bid and updates auction storage variables accordingly. If bid is placed within `extend_time` from `end_time`, extend the auction by `extend_time` to prevent front running wars at end.

```sh=
%bid {
  asset_id : nat;
} 
```

### %cancel
If `SENDER` is `seller` and auction is in progress, a call to this entrypoint will return `assets` to `owner`and return `current_bid` to `highest_bidder`. Note, if no bids were placed, the seller's deposit is simply returned. It will also delete auction data from assets big_map

```sh=
  %cancel {
    asset_id : nat;
  }
```

### %resolve 
This entrypoint checks the auction has ended and sends asset to `highest_bidder` and `current_bid` to owner. Note, if no bids were placed, the seller's deposit is simply returned. It also delete auction data from assets big_map.

```sh=
%resolve {
  asset_id : nat;
}

```


## Design Choices/Cautions

1. Only Implicit accounts can place bids.
2. **CAUTION:**   Smart contracts can configure bids. Bidders are encouraged to inspect the code of the selling contract and confirm that the first bid as well as the final reward (upon a call to `resolve_contract`) will be accepted and not be used to steal gas in the process. See https://www.notion.so/Review-report-for-English-auction-contract-iteration-2-c3610435cc1446d1b6f2b2d60dc86c8e for more details on this. 


## Future work

1. Whitelisting: Only allow certain addresses to configure auctions.
2. Bidding in FA2
3. Contract fee.
4. Bidding with permits. 
5. Full front running protection. 

# English Auction w/ FA2 bids

In this version of the NFT English Auction contract, bids are made in some FA2 token specified at contract origination as `bid_currency`. Bidders must add the auction contract as an operator for their bid token and then specify the amount they would like to bid in mutez as an argument to the `bid` entrypoint. The entrypoint will fail if the auction cannot transfer the indicated amount of `bid_currency` to itself from the bidder. 

# English Auction with permit configuration

This is an implementation of the auction contract in which the standard `Configure` entrypoint is replaced by one that accepts a batch of optional permits that can be used to configure auctions for accounts different than `SENDER.` This could serve useful for applications looking to allow users without tez to auction off their assets. See [One-step Permit](https://gitlab.com/tzip/tzip/-/merge_requests/151) for 
reference. 
