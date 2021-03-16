#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"

type fa2_token =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }
type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_token list);
  }

type bid_currency = 
  [@layout:comb]
  {
      fa2_address : address;
      token_id : token_id;
  }

type auction =
  [@layout:comb]
  {
    seller : address;
    current_bid : nat;
    start_time : timestamp;
    last_bid_time : timestamp;
    round_time : int;
    extend_time : int;
    asset : (tokens list);
    min_raise_percent : nat;
    min_raise : nat;
    end_time : timestamp;
    highest_bidder : address;
  }

type configure_param =
  [@layout:comb]
  {
    opening_price : nat;
    min_raise_percent : nat;
    min_raise : nat;
    round_time : nat;
    extend_time : nat;
    asset : (tokens list);
    start_time : timestamp;
    end_time : timestamp;
  }

type bid_param = 
  [@layout:comb]
  {
    asset_id : nat;
    bid_amount : nat;
  }

type auction_entrypoints =
  | Configure of configure_param
  | Bid of bid_param
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin

type storage =
  [@layout:comb]
  {
    pauseable_admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    bid_currency : bid_currency;
    auctions : (nat, auction) big_map
  }

type return = operation list * storage

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let address_to_contract_transfer_entrypoint(add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : (transfer list) contract)
  | Some c ->  c

let transfer_tokens_in_single_contract (from_ : address) (to_ : address) (tokens : tokens) : operation = 
  let to_tx (fa2_token : fa2_token) : transfer_destination = {
      to_ = to_;
      token_id = fa2_token.token_id;
      amount = fa2_token.amount;
   } in
   let txs = List.map to_tx tokens.fa2_batch in
   let transfer_param = [{from_ = from_; txs = txs}] in
   let c = address_to_contract_transfer_entrypoint(tokens.fa2_address) in
   (Tezos.transaction transfer_param 0mutez c) 

(*Handles transfers of tokens across FA2 Contracts*)
let transfer_tokens(tokens_list, from_, to_ : tokens list * address * address) : (operation list) =
   (List.map (transfer_tokens_in_single_contract from_ to_) tokens_list)

let fa2_fee_transfer (from_, to_, qty, bid_currency : address * address * nat * bid_currency) : operation = 
  let tx_param : tokens = 
    ({
        fa2_address = bid_currency.fa2_address;
        fa2_batch = [({token_id = bid_currency.token_id; amount = qty} : fa2_token)]
    }) in
  let op = transfer_tokens_in_single_contract from_ to_ tx_param in
  op

let get_auction_data (asset_id, storage : nat * storage) : auction =
  match (Big_map.find_opt asset_id storage.auctions) with
      None -> (failwith "Auction does not exist for given asset_id" : auction)
    | Some auction -> auction

let auction_ended (auction : auction) : bool =
  ((Tezos.now >= auction.end_time) || (* auction has passed auction time*)
   (Tezos.now > auction.last_bid_time + auction.round_time)) (*round time has passed after bid has been placed*)

let auction_started (auction : auction) : bool =
  Tezos.now >= auction.start_time

let auction_in_progress (auction : auction) : bool =
  auction_started(auction) && (not auction_ended(auction))

(*This condition is met iff no bid has been placed before the function executes*)
let first_bid (auction : auction) : bool =
  auction.highest_bidder = auction.seller

let ceil_div (numerator, denominator : nat * nat) : nat = abs ((- numerator) / (int denominator))

let valid_bid_amount (auction, token_amount : auction * nat) : bool =
  (token_amount >= (auction.current_bid + (ceil_div (auction.min_raise_percent *  auction.current_bid, 100n)))) ||
  (token_amount >= auction.current_bid + auction.min_raise)                                            ||
  ((token_amount >= auction.current_bid) && first_bid(auction))

let configure_auction(configure_param, storage : configure_param * storage) : return = begin
    (fail_if_not_admin storage.pauseable_admin (None : string option));
    (fail_if_paused storage.pauseable_admin);
    assert_msg (configure_param.end_time > configure_param.start_time, "end_time must be after start_time");
    assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "Auction time must be less than max_auction_time");

    assert_msg (configure_param.start_time >= Tezos.now, "Start_time must not have already passed");
    assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "start_time must not be greater than the sum of current time and max_config_to_start_time");

    assert_msg (configure_param.opening_price > 0n, "Opening price must be greater than 0 tokens");
    assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez");
    assert_msg (configure_param.round_time > 0n, "Round_time must be greater than 0 seconds");

    let auction_data : auction = {
      seller = Tezos.sender;
      current_bid = configure_param.opening_price;
      start_time = configure_param.start_time;
      round_time = int(configure_param.round_time);
      extend_time = int(configure_param.extend_time);
      asset = configure_param.asset;
      min_raise_percent = configure_param.min_raise_percent;
      min_raise = configure_param.min_raise;
      end_time = configure_param.end_time;
      highest_bidder = Tezos.sender;
      last_bid_time = configure_param.start_time;
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.current_id (Some auction_data) storage.auctions in
    let asset_transfers : operation list = transfer_tokens(configure_param.asset, Tezos.sender, Tezos.self_address) in
    let fee_transfer : operation = fa2_fee_transfer(Tezos.sender, Tezos.self_address, configure_param.opening_price, storage.bid_currency) in
    ((fee_transfer :: asset_transfers), {storage with auctions = updated_auctions; current_id = storage.current_id + 1n})
  end

let resolve_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.pauseable_admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (auction_ended(auction) , "Auction must have ended");
    assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez");

    let asset_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.highest_bidder) in
    let fee_transfer : operation = fa2_fee_transfer(Tezos.self_address, auction.seller, auction.current_bid, storage.bid_currency) in
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (fee_transfer :: asset_transfers, {storage with auctions = updated_auctions})
  end

let cancel_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.pauseable_admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (Tezos.sender = auction.seller, "Only seller can cancel auction");
    assert_msg (not auction_ended(auction), "Auction must not have ended");
    assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez");

    let asset_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let fee_transfer : operation = fa2_fee_transfer(Tezos.self_address, auction.highest_bidder, auction.current_bid, storage.bid_currency) in
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (fee_transfer :: asset_transfers, {storage with auctions = updated_auctions})
  end

let place_bid(asset_id, token_amount, storage : nat * nat * storage) : return = begin
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (Tezos.sender = Tezos.source, "Bidder must be an implicit account");
    (fail_if_paused storage.pauseable_admin);
    assert_msg (auction_in_progress(auction), "Auction must be in progress");
    assert_msg(Tezos.sender <> auction.seller, "Seller cannot place a bid");
    (if not valid_bid_amount(auction, token_amount) 
      then ([%Michelson ({| { FAILWITH } |} : string * (nat * nat * address * timestamp * timestamp) -> unit)] ("Invalid Bid amount", (auction.current_bid, token_amount, auction.highest_bidder, auction.last_bid_time, Tezos.now)) : unit)
      else ());
    
    let bid_self_transfer : operation = fa2_fee_transfer(Tezos.sender, Tezos.self_address, token_amount, storage.bid_currency) in
    let return_previous_bid : operation = fa2_fee_transfer(Tezos.self_address, auction.highest_bidder, auction.current_bid, storage.bid_currency) in
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let updated_auction_data = {auction with current_bid = token_amount; highest_bidder = Tezos.sender; last_bid_time = Tezos.now; end_time = new_end_time;} in
    let updated_auctions = Big_map.update asset_id (Some updated_auction_data) storage.auctions in
    ([return_previous_bid; bid_self_transfer] , {storage with auctions = updated_auctions})
  end

let admin(admin_param, storage : pauseable_admin * storage) : return =
    let u = assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez") in
    let ops, pauseable_admin = pauseable_admin(admin_param, storage.pauseable_admin) in
    let new_storage = { storage with pauseable_admin = pauseable_admin; } in
    ops, new_storage

let english_auction_fa2_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | Bid bid_param -> place_bid(bid_param.asset_id, bid_param.bid_amount, storage)
    | Cancel asset_id -> cancel_auction(asset_id, storage)
    | Resolve asset_id -> resolve_auction(asset_id, storage)
    | Admin a -> admin(a, storage)
