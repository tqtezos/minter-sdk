#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"

type bid_currency = global_token_id

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
  | Update_allowed of allowlist_entrypoints

#if !FEE

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    bid_currency : bid_currency;
    auctions : (nat, auction) big_map;
    allowlist : allowlist;
  }

#else

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    bid_currency : bid_currency;
    auctions : (nat, auction) big_map;
    allowlist : allowlist;
    fee : fee_data;
  }

#endif

type return = operation list * storage

let transfer_tokens_in_single_contract (transfer_param, fa2: (transfer list) * address) : operation =
  let c = address_to_contract_transfer_entrypoint(fa2) in
  (Tezos.transaction transfer_param 0mutez c)

let transfer_tokens_in_single_contract_to_address (from_ : address) (to_ : address) (tokens : tokens) : operation =
  let to_tx (fa2_tokens : fa2_tokens) : transfer_destination = {
      to_ = to_;
      token_id = fa2_tokens.token_id;
      amount = fa2_tokens.amount;
   } in
   let txs = List.map to_tx tokens.fa2_batch in
   let transfer_list : transfer list = [{from_ = from_; txs = txs}] in
   (transfer_tokens_in_single_contract (transfer_list, tokens.fa2_address))

let single_fa2_transfer (from_, to_, qty, fa2 : address * address * nat * bid_currency) : operation =
  let single_transfer_tx : transfer = {
    from_ = from_;
    txs = [{
      to_ = to_;
      token_id = fa2.token_id;
      amount = qty;
    }];
  } in
  let single_transfer : operation = transfer_tokens_in_single_contract ([single_transfer_tx], fa2.fa2_address) in
  single_transfer

(*Handles transfers of tokens across FA2 Contracts*)
let transfer_tokens(tokens_list, from_, to_ : tokens list * address * address) : (operation list) =
   (List.map (transfer_tokens_in_single_contract_to_address from_ to_) tokens_list)

let get_auction_data (asset_id, storage : nat * storage) : auction =
  match (Big_map.find_opt asset_id storage.auctions) with
      None -> (failwith "AUCTION_DOES_NOT_EXIST" : auction)
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

let valid_bid_amount (auction, token_amount : auction * nat) : bool =
  (token_amount >= (auction.current_bid + (percent_of_bid_nat (auction.min_raise_percent, auction.current_bid)))) ||
  (token_amount >= auction.current_bid + auction.min_raise)                                            ||
  ((token_amount >= auction.current_bid) && first_bid(auction))

let check_allowlisted (configure_param, allowlist : configure_param * allowlist) : unit = begin
  List.iter
    (fun (token : tokens) ->
      check_tokens_allowed (token, allowlist, "ASSET_NOT_ALLOWED")
    )
    configure_param.asset;
  unit end

let configure_auction(configure_param, storage : configure_param * storage) : return = begin
#if !CANCEL_ONLY_ADMIN
    (fail_if_not_admin storage.admin);
#endif
    (fail_if_paused storage.admin);
    check_allowlisted(configure_param, storage.allowlist);
#if FEE
    assert_msg (storage.fee.fee_percent <= 100n, "INVALID_FEE");
#endif

    assert_msg (configure_param.end_time > configure_param.start_time, "INVALID_END_TIME");
    assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "INVALID_AUCTION_TIME");

    assert_msg (configure_param.start_time >= Tezos.now, "INVALID_START_TIME");
    assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "MAX_CONFIG_TO_START_TIME_VIOLATED");

    assert_msg (configure_param.opening_price > 0n, "INVALID_OPENING_PRICE");
    assert_msg (Tezos.amount = 0mutez, "INVALID_AMOUNT");
    assert_msg (configure_param.round_time > 0n, "INVALID_ROUND_TIME");
    assert_msg(configure_param.min_raise_percent > 0n && configure_param.min_raise > 0n, "INVALID_RAISE_CONFIGURATION");

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
    (asset_transfers, {storage with auctions = updated_auctions; current_id = storage.current_id + 1n})
  end

let resolve_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (auction_ended(auction) , "AUCTION_NOT_ENDED");
    tez_stuck_guard("RESOLVE");
    let asset_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.highest_bidder) in

#if !FEE
    let op_list : operation list = if first_bid(auction) then
      asset_transfers else
      let send_final_bid : operation = single_fa2_transfer(Tezos.self_address, auction.seller, auction.current_bid, storage.bid_currency) in
      (send_final_bid :: asset_transfers) in
#else

    let op_list : operation list = if first_bid(auction) then
      asset_transfers else
      let fee : nat = percent_of_price_nat (storage.fee.fee_percent, auction.current_bid) in //ceiling not used
      let final_price_minus_fee : nat =  (match (is_nat (auction.current_bid - fee)) with
        | Some adjusted_price -> adjusted_price
        | None -> (failwith "INVALID_FEE" : nat)) in
      let pay_fee_tx : transfer_destination = {
          to_ = storage.fee.fee_address;
          token_id = storage.bid_currency.token_id;
          amount = fee;
        } in
      let send_final_bid_minus_fee_tx : transfer_destination = {
          to_ = auction.seller;
          token_id = storage.bid_currency.token_id;
          amount = final_price_minus_fee;
      } in
      let txs : transfer list = [{
        from_ = Tezos.self_address;
        txs = [pay_fee_tx; send_final_bid_minus_fee_tx];
      }] in
      let payments : operation = transfer_tokens_in_single_contract (txs, storage.bid_currency.fa2_address) in
      (payments :: asset_transfers) in
#endif
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let cancel_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
          else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    assert_msg (not auction_ended(auction), "AUCTION_ENDED");
    assert_msg (Tezos.amount = 0mutez, "INVALID_AMOUNT");

    let asset_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let op_list : operation list = if first_bid(auction) then
      asset_transfers else
      let return_bid : operation = single_fa2_transfer(Tezos.self_address, auction.highest_bidder, auction.current_bid, storage.bid_currency) in
      (return_bid :: asset_transfers) in
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let place_bid(asset_id, token_amount, storage : nat * nat * storage) : return = begin
    let auction : auction = get_auction_data(asset_id, storage) in
    assert_msg (Tezos.sender = Tezos.source, "BIDDER_NOT_IMPLICIT");
    (fail_if_paused storage.admin);
    assert_msg (auction_in_progress(auction), "NOT_IN_PROGRESS");
    assert_msg(Tezos.sender <> auction.seller, "SEllER_CANT_BID");
    assert_msg(Tezos.sender <> auction.highest_bidder, "NO_SELF_OUTBIDS");
    (if not valid_bid_amount(auction, token_amount)
      then ([%Michelson ({| { FAILWITH } |} : string * (nat * nat * address * timestamp * timestamp) -> unit)] ("INVALID_BID_AMOUNT", (auction.current_bid, token_amount, auction.highest_bidder, auction.last_bid_time, Tezos.now)) : unit)
      else ());

    let bid_self_transfer : operation = single_fa2_transfer(Tezos.sender, Tezos.self_address, token_amount, storage.bid_currency) in
    let op_list : operation list = if first_bid(auction) then
      [bid_self_transfer] else
      let return_previous_bid : operation = single_fa2_transfer(Tezos.self_address, auction.highest_bidder, auction.current_bid, storage.bid_currency) in
      [return_previous_bid; bid_self_transfer] in
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let updated_auction_data = {auction with current_bid = token_amount; highest_bidder = Tezos.sender; last_bid_time = Tezos.now; end_time = new_end_time;} in
    let updated_auctions = Big_map.update asset_id (Some updated_auction_data) storage.auctions in
    (op_list , {storage with auctions = updated_auctions})
  end

let admin(admin_param, storage : pauseable_admin * storage) : return =
    let u = assert_msg (Tezos.amount = 0mutez, "INVALID_AMOUNT") in
    let ops, admin = pauseable_admin(admin_param, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage

let update_allowed(allowlist_param, storage : allowlist_entrypoints * storage) : return =
#if !ALLOWLIST_ENABLED
    [%Michelson ({| { NEVER } |} : never -> return)] allowlist_param
#else
    let u : unit = fail_if_not_admin(storage.admin) in
    let allowlist_storage = update_allowed (allowlist_param, storage.allowlist) in
    ([] : operation list), { storage with allowlist = allowlist_storage }
#endif

let english_auction_fa2_main (p,storage : auction_entrypoints * storage) : return = 
  let u : unit = tez_stuck_guard("ANY_ENTRYPOINT") in 
  match p with
    | Configure config -> configure_auction(config, storage)
    | Bid bid_param -> place_bid(bid_param.asset_id, bid_param.bid_amount, storage)
    | Cancel asset_id -> cancel_auction(asset_id, storage)
    | Resolve asset_id -> resolve_auction(asset_id, storage)
    | Admin a -> admin(a, storage)
    | Update_allowed a -> update_allowed(a, storage)
