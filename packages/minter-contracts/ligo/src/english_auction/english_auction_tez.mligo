#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../common.mligo"

type auction =
  [@layout:comb]
  {
    seller : address;
    current_bid : tez;
    start_time : timestamp;
    last_bid_time : timestamp;
    round_time : int;
    extend_time : int;
    asset : (tokens list);
    min_raise_percent : nat;
    min_raise : tez;
    end_time : timestamp;
    highest_bidder : address;
  }

type configure_param =
  [@layout:comb]
  {
    opening_price : tez;
    min_raise_percent : nat;
    min_raise : tez;
    round_time : nat;
    extend_time : nat;
    asset : (tokens list);
    start_time : timestamp;
    end_time : timestamp;
  }

type auction_without_configure_entrypoints =
  | Bid of nat
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin

type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

#if !FEE

type storage =
  [@layout:comb]
  {
    pauseable_admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map
  }

#else 

type storage =
  [@layout:comb]
  {
    pauseable_admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map;
    fee : fee_data;
  }

#endif

type return = operation list * storage

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

let rec tokens_list_to_operation_list_append (from_, to_, tokens_list, op_list : address * address * tokens list * (operation list)) :  (operation list) =
  let tokens = List.head_opt tokens_list in 
  let new_tokens_list = List.tail_opt tokens_list in 
  match tokens with 
    | Some t -> 
        let op = (transfer_tokens_in_single_contract from_ to_ t) in 
        let new_op_list : operation list = (op :: op_list) in
        (match new_tokens_list with 
          | Some tl -> tokens_list_to_operation_list_append(from_, to_, tl, new_op_list)
          | None -> (failwith "INTERNAL_ERROR" : operation list))
    | None -> op_list

let get_auction_data ((asset_id, storage) : nat * storage) : auction =
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

let valid_bid_amount (auction : auction) : bool =
  (Tezos.amount >= (auction.current_bid + (percent_of_bid_tez (auction.min_raise_percent, auction.current_bid)))) ||
  (Tezos.amount >= auction.current_bid + auction.min_raise)                                            ||
  ((Tezos.amount >= auction.current_bid) && first_bid(auction))

let configure_auction_storage(configure_param, seller, storage : configure_param * address * storage ) : storage = begin
    (fail_if_not_admin storage.pauseable_admin);
    (fail_if_paused storage.pauseable_admin);

    assert_msg (configure_param.end_time > configure_param.start_time, "end_time must be after start_time");
    assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "Auction time must be less than max_auction_time");
    
    assert_msg (configure_param.start_time >= Tezos.now, "Start_time must not have already passed");
    assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "start_time must not be greater than the sum of current time and max_config_to_start_time");
    
    assert_msg (configure_param.opening_price > 0mutez, "Opening price must be greater than 0mutez");
    assert_msg (Tezos.amount = 0mutez, "Amount sent must be 0mutez");
    assert_msg (configure_param.round_time > 0n, "Round_time must be greater than 0 seconds");

    let auction_data : auction = {
      seller = seller;
      current_bid = configure_param.opening_price;
      start_time = configure_param.start_time;
      round_time = int(configure_param.round_time);
      extend_time = int(configure_param.extend_time);
      asset = configure_param.asset;
      min_raise_percent = configure_param.min_raise_percent;
      min_raise = configure_param.min_raise;
      end_time = configure_param.end_time;
      highest_bidder = seller;
      last_bid_time = configure_param.start_time; 
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.current_id (Some auction_data) storage.auctions in
    {storage with auctions = updated_auctions; current_id = storage.current_id + 1n}
  end

let configure_auction(configure_param, storage : configure_param * storage) : return = 
#if FEE
  let u : unit = assert_msg (storage.fee.fee_percent <= 100n, "Fee_percent must be less than 100%. Please originate another contract.") in
#endif
  let new_storage = configure_auction_storage(configure_param, Tezos.sender, storage) in
  let fa2_transfers : operation list = transfer_tokens(configure_param.asset, Tezos.sender, Tezos.self_address) in
  (fa2_transfers, new_storage)
 

let resolve_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.pauseable_admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (auction_ended(auction) , "Auction must have ended");
    assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.highest_bidder) in
    let seller_contract : unit contract = resolve_contract(auction.seller) in
    
#if !FEE 

    let op_list = if first_bid(auction) then 
      fa2_transfers else 
      let send_final_bid : operation = Tezos.transaction unit auction.current_bid seller_contract in
      (send_final_bid :: fa2_transfers) in

#else 
    let fee_contract : unit contract = resolve_contract(storage.fee.fee_address) in
    let op_list = if first_bid(auction) then 
      fa2_transfers else 
      let fee : tez = percent_of_bid_tez (storage.fee.fee_percent, auction.current_bid) in 
      let pay_fee : operation = Tezos.transaction unit fee fee_contract in 
      let send_final_bid_minus_fee : operation = Tezos.transaction unit (auction.current_bid - fee) seller_contract in
      (pay_fee :: send_final_bid_minus_fee :: fa2_transfers) in

#endif

    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let cancel_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.pauseable_admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (Tezos.sender = auction.seller, "Only seller can cancel auction");
    assert_msg (not auction_ended(auction), "Auction must not have ended");
    assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let highest_bidder_contract : unit contract = resolve_contract(auction.highest_bidder) in
    let op_list : operation list = if first_bid(auction) then 
      fa2_transfers else  
      let return_bid : operation = Tezos.transaction unit auction.current_bid highest_bidder_contract in 
      (return_bid :: fa2_transfers) in
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let place_bid(asset_id, storage : nat * storage) : return = begin
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (Tezos.sender = Tezos.source, "Bidder must be an implicit account");
    (fail_if_paused storage.pauseable_admin);
    assert_msg (auction_in_progress(auction), "Auction must be in progress");
    assert_msg(Tezos.sender <> auction.seller, "Seller cannot place a bid");
    (if not valid_bid_amount(auction) 
      then ([%Michelson ({| { FAILWITH } |} : string * (tez * tez * address * timestamp * timestamp) -> unit)] ("Invalid Bid amount", (auction.current_bid, Tezos.amount, auction.highest_bidder, auction.last_bid_time, Tezos.now)) : unit)
      else ());

    let highest_bidder_contract : unit contract = resolve_contract(auction.highest_bidder) in
    let op_list : operation list = if first_bid(auction) then 
      ([] : operation list) else 
      let return_bid : operation = Tezos.transaction unit auction.current_bid highest_bidder_contract in
      [return_bid] in
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let updated_auction_data = {auction with current_bid = Tezos.amount; highest_bidder = Tezos.sender; last_bid_time = Tezos.now; end_time = new_end_time;} in
    let updated_auctions = Big_map.update asset_id (Some updated_auction_data) storage.auctions in
    (op_list , {storage with auctions = updated_auctions})
  end

let admin(admin_param, storage : pauseable_admin * storage) : return =
    let u = assert_msg (Tezos.amount = 0mutez, "Amount must be 0mutez") in
    let ops, pauseable_admin = pauseable_admin(admin_param, storage.pauseable_admin) in
    let new_storage = { storage with pauseable_admin = pauseable_admin; } in
    ops, new_storage

let english_auction_tez_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return = 
  match p with
    | Bid asset_id -> place_bid(asset_id, storage)
    | Cancel asset_id -> cancel_auction(asset_id, storage)
    | Resolve asset_id -> resolve_auction(asset_id, storage)
    | Admin a -> admin(a, storage)

let english_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> english_auction_tez_no_configure(ai, storage)
