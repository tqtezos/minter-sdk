# Bonding Curve Multiunit Mint Auction

## Bonding Curve and Bidding
This auction allows a seller to mint unique nfts to auction bidders. Winning bidders are selected if their bid meets a minimum threshold determined by a bonding curve, which we understand in this context as a non-decreasing function `f(q) = minimum winning bid` where `f: Nat -> Tez` and `q = the total number of offers`, defined as a lambda stored in contract storage. 

After all bids are received, winning bids are defined as the largest set `X` of `N`  highest bids that satisfy the condition that `P(B_m) >= f(N)` where `B_m` is the minimum priced bid in that set `X` and `P(B_m)` is the price of that bid. The size of set `X` is defined as `The sum of the number of offers of bids in X`. 

### Increase Bid

After bidding, a user can also increase their previous bid, granted their new bid is greater than or equal to the previous bid in both quantity and price. In this case, they only have to pay the difference betweent their new bid and their old bid. They place such a bid by setting the field in the bid param to `Some bid_heap_key`  where  `bid_heap_key` is the big_map key for where the previous bid of their's that they would like to increase is located. 


## Returning Bids and Offers
A bid is defined as some number of offers `q` at a price `p`. An auction terminates according to the same conditions found in the standard English auction contract, with the added condition that all invalid bids and offers are manually returned to bidders, by calling the `Return_old_bids` and `Return_old_offers` entrypoints. The latter is only necessary if only a fraction of the offers in a minimum bid are allowed in order to satisfy the bonding curve condition. 

## Resolve

After an auction terminates, the auction can be resolved by calling `Resolve`. `Resolve` will handle the auction fees and set the `winning price = WP = f(N)`. We define `TotalFees` as the net fees bidders will have paid to the contract after the auction is finished. `TotalFees` is determined by `N * f(N)` where `N` is the number of winning bids and `f(N)` is the bonding curve applied to N. `Resolve` will send `TotalFees` to the profit address defined in storage. 

## Payout Winners

Mints the NFTs with highest bids receiveing the lowest `token_id`s, starting at `token_id` 1.
Also, returns to bidders the excess that they paid, or the amount they bid over the winning price `WP`.  