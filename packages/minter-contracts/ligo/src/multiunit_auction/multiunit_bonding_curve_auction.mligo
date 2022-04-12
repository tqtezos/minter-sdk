#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../common.mligo"

type auction_id = nat

type bid_param = 
 [@layout:comb]
 {  
    auction_id : nat;
    quantity : nat;
    price : tez;
 }

type bid = 
  [@layout:comb]
  {
    bidder : address;
    quantity : nat;
#if OFFCHAIN_BID
    is_offchain : bool;
#endif
    price : tez;
  }

type permit_multiunit_bid_param =
  [@layout:comb]
  {
    bid_param : bid_param;
    permit : permit;
  } 

type asset_token =
  [@layout:comb]
  {
    fa2_address : address;
    token_id : token_id;
    amount_ : nat;
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
    asset : asset_token;
    assets_sent : nat;
    end_time : timestamp;
    bonding_curve : nat;
    bid_index : nat;
    num_offers : nat; 
    pq_pair : (tez * nat) option;  (*p = min_price for valid bid, q = max satisfiable at p*)
  }

type configure_param =
  [@layout:comb]
  {
    minimum_price : tez;
    round_time : nat;
    extend_time : nat;
    asset : asset_token;
    start_time : timestamp;
    end_time : timestamp;
    bonding_curve : nat;
  }

type auction_without_configure_entrypoints =
  | Bid of bid_param
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin
#if OFFCHAIN_BID
  | Offchain_bid of permit_bid_param
#endif

type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

type bid_heap_key = 
  [@layout:comb]
  {
   auction_id : auction_id;
   bid_index : nat; 
  }

type bid_heap =  (bid_heap_key, bid) big_map

type heap_sizes = (auction_id, nat) big_map

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    auction_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map;
    bonding_curve_index : nat;
    bonding_curves : (nat, tez -> nat) big_map;
    bids : bid_heap;
    heap_sizes : heap_sizes;
  }

type return = operation list * storage

let parent (i : nat ) : nat = 
   if i = 0n then 0n
   else (abs (i - 1n))/2n

let left_child (i : nat) : nat = 
  (2n * i) + 1n

let right_child (i : nat) : nat =
  (2n * i) + 2n

let swap_heap_keys (i_key, j_key, i_bid, j_bid, bid_heap : bid_heap_key * bid_heap_key * bid * bid * bid_heap) : bid_heap =
  let i_replaced_heap : bid_heap = 
      Big_map.update i_key (Some j_bid) bid_heap in 
  let ij_replaced_heap : bid_heap = 
      Big_map.update j_key (Some i_bid) i_replaced_heap in 
  ij_replaced_heap

let get_bid (index_key, bid_heap : bid_heap_key * bid_heap) : bid = 
  match (Big_map.find_opt index_key bid_heap) with 
     Some bid -> bid 
   | None -> (failwith "HEAP_GET_FAILS" : bid)

let get_min (auction_id, bid_heap : auction_id * bid_heap) : bid = 
  let min_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in 
  get_bid(min_key, bid_heap)

let rec maintain_min_heap (bid_heap, current_key, current_bid : bid_heap * bid_heap_key * bid) : bid_heap =
  let parent_key : bid_heap_key = {auction_id = current_key.auction_id; bid_index = parent(current_key.bid_index);} in
  let parent_bid : bid = get_bid(parent_key, bid_heap) in 
  (
  if current_key.bid_index = 0n || parent_bid.price <= current_bid.price
  then bid_heap
  else 
       let bid_heap : bid_heap = swap_heap_keys(parent_key, current_key, parent_bid, current_bid, bid_heap) in 
       maintain_min_heap (bid_heap, parent_key, parent_bid)
  )

let get_heap_size(auction_id, heap_sizes : auction_id * heap_sizes) : nat = 
    match (Big_map.find_opt auction_id heap_sizes) with 
        Some size -> size 
      | None -> (failwith "INVALID_HEAP_SIZE" : nat)

let update_heap_size(auction_id, heap_sizes, new_size : auction_id * heap_sizes * nat) : heap_sizes = 
    Big_map.update auction_id (Some new_size) heap_sizes
   
let insert_bid (bid, bid_heap, auction_id, new_heap_size : bid * bid_heap * auction_id * nat) : bid_heap =
  (*insert bid at end of heap*)
  let bid_key : bid_heap_key = {auction_id = auction_id; bid_index = new_heap_size;} in
  let bid_heap : bid_heap = Big_map.add bid_key bid bid_heap in 

  (*maintain min heap property*)
  let bid_heap : bid_heap = maintain_min_heap (bid_heap, bid_key, bid) in 
  bid_heap

let rec min_heapify (index_key, bid_heap, heap_size : bid_heap_key * bid_heap * nat) : bid_heap = 
   let index_bid : bid = get_bid(index_key, bid_heap) in

   let l_index : nat = left_child(index_key.bid_index) in 
   let l_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = l_index;} in
   let l_bid : bid = get_bid(l_key, bid_heap) in 

   let r_index : nat = right_child(index_key.bid_index) in 
   let r_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = r_index;} in
   let r_bid : bid = get_bid(r_key, bid_heap) in 

   let (smallest_key, smallest_bid) : bid_heap_key * bid = 
     if l_index < heap_size && l_bid < index_bid
     then (l_key, l_bid) 
     else (index_key, index_bid)
    in 

   let (smallest_key, smallest_bid) : bid_heap_key * bid = 
     if r_index < heap_size && r_bid < smallest_bid
     then (r_key, r_bid)
     else (smallest_key, smallest_bid)
    in

   if smallest_key <> index_key 
   then 
        let bid_heap : bid_heap = swap_heap_keys(smallest_key, index_key, smallest_bid, index_bid, bid_heap) in 
        min_heapify(smallest_key, bid_heap, heap_size)
   else bid_heap
   

let extract_min (bid_heap, auction_id, heap_size : bid_heap * auction_id * nat) : bid option * bid_heap * nat= 
    if heap_size <= 0n 
    then ((None : bid option), bid_heap, heap_size)
    else  
          let new_heap_size : nat = abs(heap_size - 1n) in 
          let min_bid : bid = get_min(auction_id, bid_heap) in 
          
          let percolate_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = new_heap_size;} in
      
          let (percolate_bid_option, bid_heap) : bid option * bid_heap = 
               Big_map.get_and_update percolate_bid_key (None : bid option) bid_heap in 
          let percolate_bid : bid =  match percolate_bid_option with 
               Some bid -> bid 
            |  None -> (failwith "HEAP_GET_FAILS" : bid )
            in 
          
          let min_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in 

          let (min_bid_option, bid_heap) : bid option * bid_heap = 
             Big_map.get_and_update min_bid_key (Some percolate_bid) bid_heap in 
          let min_bid : bid =  match min_bid_option with 
               Some bid -> bid 
            |  None -> (failwith "HEAP_GET_FAILS" : bid )
            in 
          let new_heap : bid_heap = min_heapify(min_bid_key, bid_heap, new_heap_size) in 
          ((Some min_bid), new_heap, new_heap_size)

let transfer_asset_tokens (from_, to_, asset_token : address * address * asset_token) : operation list = 
  let c = address_to_contract_transfer_entrypoint(asset_token.fa2_address) in
  let transfer_param = [{from_ = from_; txs = [{to_ = to_; token_id = asset_token.token_id; amount = asset_token.amount_}]}] in 
  let op : operation = Tezos.transaction transfer_param 0mutez c in
  [op]

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

let get_auction_data ((auction_id, storage) : nat * storage) : auction =
  match (Big_map.find_opt auction_id storage.auctions) with
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
  auction.bid_index = 0n

let dont_return_bid (auction : auction) : bool =
  first_bid(auction)

#if OFFCHAIN_BID
  || auction.last_bid_offchain 
#endif

let bid_amount_sent (auction, bid_param: auction * bid_param) : bool =
  Tezos.amount = bid_param.price * bid_param.quantity (*If bid is offchian, no need to send tez*)

let configure_auction_storage(configure_param, seller, storage : configure_param * address * storage ) : storage = begin
#if !CANCEL_ONLY_ADMIN
    (fail_if_not_admin storage.admin);
#endif
    (fail_if_paused storage.admin);

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
      assets_sent = 0n;
      end_time = configure_param.end_time;
      last_bid_time = configure_param.start_time;
      bonding_curve = configure_param.bonding_curve;
      bid_index = 0n;
      pq_pair = (None : (tez * nat) option); 
      num_offers = 0n;
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.auction_id (Some auction_data) storage.auctions in
    {storage with auctions = updated_auctions; auction_id = storage.auction_id + 1n}
  end

let configure_auction(configure_param, storage : configure_param * storage) : return =
  let new_storage = configure_auction_storage(configure_param, Tezos.sender, storage) in
  let fa2_transfers : operation list = transfer_asset_tokens(Tezos.sender, Tezos.self_address, configure_param.asset) in
  (fa2_transfers, new_storage)

let resolve_auction(auction_id, storage : nat * storage) : return = begin
  (([] : operation list), storage)
 end

let cancel_auction(auction_id, storage : nat * storage) : return = begin
    (([] : operation list), storage)
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
    (if (not bid_amount_sent(auction, bid_param) 
#if OFFCHAIN_BID 
        && not is_offchain
#endif
        ) || bid_param.price < auction.minimum_price ||  
          (match auction.pq_pair with 
              Some pq -> bid_param.price < pq.0 
            | None -> false
          )
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

    let current_heap_size : nat = get_heap_size(bid_param.auction_id, storage.heap_sizes) in 
    let new_heap_size : nat = current_heap_size + 1n in 

    let bid_heap : bid_heap = insert_bid(new_bid, storage.bids, bid_param.auction_id, new_heap_size) in

    let updated_num_offers : nat = auction.num_offers + new_bid.quantity in 

    let new_heap_size_bm : heap_sizes = update_heap_size(bid_param.auction_id, storage.heap_sizes, new_heap_size) in 

    let updated_auction_data : auction = {auction with last_bid_time = Tezos.now; end_time = new_end_time; 
                                bid_index = auction.bid_index + 1n; 
                                num_offers = updated_num_offers;} in
    let updated_auctions = Big_map.update bid_param.auction_id (Some updated_auction_data) storage.auctions in
    (([] : operation list) , {storage with auctions = updated_auctions; bids = bid_heap; heap_sizes = new_heap_size_bm;})
  end

let place_bid_onchain(bid_param, storage : bid_param * storage) : return = begin
    let bidder = Tezos.sender in 
    let auction : auction = get_auction_data(bid_param.auction_id, storage) in 
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
    let auction : auction = get_auction_data(bid_param.auction_id, storage) in
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

let multiunit_bonding_curve_auction_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid bid_param -> place_bid_onchain(bid_param, storage)
    | Cancel auction_id -> cancel_auction(auction_id, storage)
    | Resolve auction_id -> resolve_auction(auction_id, storage)
    | Admin a -> admin(a, storage)
#if OFFCHAIN_BID
    | Offchain_bid permit -> bid_with_permit(permit, storage)
#endif

let multiunit_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> multiunit_bonding_curve_auction_no_configure(ai, storage)
