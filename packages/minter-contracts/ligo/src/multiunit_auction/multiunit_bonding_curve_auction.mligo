#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../allowlist_common.mligo"
#include "../common.mligo"

type bid_param = 
 [@layout:comb]
 {  
    asset_id : nat;
    quantity : nat;
    price : tez;
 }

type bid = 
  [@layout:comb]
  {
    bidder : address;
    quantity : nat;
    price : tez;
#if OFFCHAIN_BID
    is_offchain : bool;
#endif
  }

type bids = bid list

type permit_multiunit_bid_param =
  [@layout:comb]
  {
    bid_param : bid_param;
    permit : permit;
  } 

type auction =
  [@layout:comb]
  {
    seller : address;
    minimum_price : tez;
    start_time : timestamp;
    last_bid_time : timestamp;
    round_time : int;
    extend_time : int;
    asset : (tokens list);
    end_time : timestamp;
    bids : bids;
    bonding_curve : nat;
  }

type configure_param =
  [@layout:comb]
  {
    minimum_price : tez;
    round_time : nat;
    extend_time : nat;
    asset : (tokens list);
    start_time : timestamp;
    end_time : timestamp;
    bonding_curve : nat;
  }

type auction_without_configure_entrypoints =
  | Bid of bid_param
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin
  | Update_allowed of allowlist_entrypoints
#if OFFCHAIN_BID
  | Offchain_bid of permit_bid_param
#endif

type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

#if !FEE

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map;
    allowlist : allowlist;
    bonding_curves : (nat, nat -> tez) big_map;
#else
    fee : fee_data;
#endif
  }

type return = operation list * storage

let transfer_tokens_in_single_contract (from_ : address) (to_ : address) (tokens : tokens) : operation =
  let to_tx (fa2_tokens : fa2_tokens) : transfer_destination = {
      to_ = to_;
      token_id = fa2_tokens.token_id;
      amount = fa2_tokens.amount;
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
  List.length auction.bids = 0n

let dont_return_bid (auction : auction) : bool =
  first_bid(auction)

#if OFFCHAIN_BID
  || auction.last_bid_offchain 
#endif

let bid_amount_sent (auction, bid_param: auction * bid_param) : bool =
  Tezos.amount = bid_param.price * bid_param.quantity (*If bid is offchian, no need to send tez*)


let check_allowlisted (configure_param, allowlist : configure_param * allowlist) : unit = begin
  List.iter
    (fun (token : tokens) ->
      check_tokens_allowed (token, allowlist, "ASSET_NOT_ALLOWED")
    )
    configure_param.asset;
  unit end

let configure_auction_storage(configure_param, seller, storage : configure_param * address * storage ) : storage = begin
#if !CANCEL_ONLY_ADMIN
    (fail_if_not_admin storage.admin);
#endif
    (fail_if_paused storage.admin);
    check_allowlisted(configure_param, storage.allowlist);

    assert_msg (configure_param.end_time > configure_param.start_time, "INVALID_END_TIME");
    assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "INVALID_AUCTION_TIME");

    assert_msg (configure_param.start_time >= Tezos.now, "INVALID_START_TIME");
    assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "MAX_CONFIG_TO_START_TIME_VIOLATED");

    assert_msg (configure_param.minimum_price > 0mutez, "INVALID_MINIMUM_PRICE");
    tez_stuck_guard("CONFIGURE");
    assert_msg (configure_param.round_time > 0n, "INVALID_ROUND_TIME");

    assert_msg(Big_map.mem configure_param.bonding_curve storage.bonding_curves, "INVALID_BONDING_CURVE");

    let auction_data : auction = {
      seller = seller;
      minimum_price = configure_param.minimum_price;
      start_time = configure_param.start_time;
      round_time = int(configure_param.round_time);
      extend_time = int(configure_param.extend_time);
      asset = configure_param.asset;
      end_time = configure_param.end_time;
      last_bid_time = configure_param.start_time;
      bids = ([] : bids);
      bonding_curve = configure_param.bonding_curve;
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.current_id (Some auction_data) storage.auctions in
    {storage with auctions = updated_auctions; current_id = storage.current_id + 1n}
  end

let configure_auction(configure_param, storage : configure_param * storage) : return =
#if FEE
  let u : unit = assert_msg (storage.fee.fee_percent <= 100n, "INVALID_FEE") in
#endif
  let new_storage = configure_auction_storage(configure_param, Tezos.sender, storage) in
  let fa2_transfers : operation list = transfer_tokens(configure_param.asset, Tezos.sender, Tezos.self_address) in
  (fa2_transfers, new_storage)

let resolve_auction(asset_id, storage : nat * storage) : return = begin
  (([] : operation list), storage)
 end

let cancel_auction(asset_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(asset_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
          else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    assert_msg (not auction_ended(auction), "AUCTION_ENDED");
    tez_stuck_guard("CANCEL");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let op_list : operation list = 
      List.fold
      (fun(ops, bid : operation list * bid) -> 
         let total_tez : tez = bid.quantity * bid.price in 
         let transfer_op : operation = transfer_tez(total_tez, bid.bidder) in 
         transfer_op :: ops
      )
      auction.bids 
      fa2_transfers
      in 
    let updated_auctions = Big_map.remove asset_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let place_bid(  bid_param
              , auction 
              , bidder 
              , storage
#if OFFCHAIN_BID
              , is_offchain
#endif
              : bid_param
              * auction 
              * address 
              * storage
#if OFFCHAIN_BID
              * bool
#endif
             ) : return = begin

    assert_msg (Tezos.sender = Tezos.source, "CALLER_NOT_IMPLICIT");
    (fail_if_paused storage.admin);
    assert_msg (auction_in_progress(auction), "NOT_IN_PROGRESS");
    assert_msg(bidder <> auction.seller, "SEllER_CANT_BID");
    assert_msg(bid_param.price >= auction.minimum_price, "BID_BELOW_MINIMUM_PRICE");
    (if not bid_amount_sent(auction, bid_param)
#if OFFCHAIN_BID 
        && not is_offchain
#endif
      then ([%Michelson ({| { FAILWITH } |} : string * (address * tez * timestamp * timestamp) -> unit)] ("INVALID_BID_AMOUNT", (bidder, bid_param.price, auction.last_bid_time, Tezos.now)) : unit)
      else ());
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let new_bid : bid = 
      ({
        quantity = bid_param.quantity;
        price = bid_param.price;
        bidder = bidder;
#if OFFCHAIN_BID
        is_offchain = is_offchain;
#endif
      } : bid) in 
    let updated_auction_data = {auction with 
                                last_bid_time = Tezos.now; end_time = new_end_time;
                                bids = new_bid :: auction.bids
                               } in
    let updated_auctions = Big_map.update bid_param.asset_id (Some updated_auction_data) storage.auctions in
    (([] : operation list) , {storage with auctions = updated_auctions})
  end

let place_bid_onchain(bid_param, storage : bid_param * storage) : return = begin
    let bidder = Tezos.sender in 
    let auction : auction = get_auction_data(bid_param.asset_id, storage) in 
    let bid_placed_offchain : bool = false in 
    let (ops, new_storage) = place_bid(bid_param, auction, bidder, storage
#if OFFCHAIN_BID
      , bid_placed_offchain
#endif
      ) in 
    (ops, new_storage)
  end

#if OFFCHAIN_BID
let place_bid_offchain(bid_param, bidder, storage : bid_param * address * storage) : return = begin
    let auction : auction = get_auction_data(bid_param.asset_id, storage) in
    let bid_placed_offchain : bool = true in 
    let (ops, new_storage) = place_bid(bid_param, auction, bidder, storage, bid_placed_offchain ) in 
    (ops, new_storage)
  end

let bid_with_permit (p, storage : permit_multiunit_bid_param * storage)  : return = begin 
    fail_if_not_admin(storage.admin);
    let {bid_param = bid_param;
         permit = permit; } = p in 
    let param_hash = Crypto.blake2b (Bytes.pack bid_param) in 
    let v : unit = check_permit (permit, 0n, param_hash) in  (*Always set counter to 0*)
    let bidder = address_from_key (p.permit.signerKey) in
    let (ops, storage) = place_bid_offchain(bid_param, bidder, storage) in 
    (ops, storage)
  end
#endif

let admin(admin_param, storage : pauseable_admin * storage) : return =
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

let multiunit_bonding_curve_auction_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid bid_param -> place_bid_onchain(bid_param, storage)
    | Cancel asset_id -> cancel_auction(asset_id, storage)
    | Resolve asset_id -> resolve_auction(asset_id, storage)
    | Admin a -> admin(a, storage)
    | Update_allowed a -> update_allowed(a, storage)
#if OFFCHAIN_BID
    | Offchain_bid permit -> bid_with_permit(permit, storage)
#endif

let multiunit_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> multiunit_bonding_curve_auction_no_configure(ai, storage)