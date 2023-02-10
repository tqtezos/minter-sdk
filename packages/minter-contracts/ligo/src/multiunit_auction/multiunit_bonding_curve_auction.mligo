#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/simple_admin.mligo"
#include "../minter_collection/nft/fa2_multi_nft_manager.mligo"
#include "../common.mligo"

type auction_id = nat

type bid_id = nat

type bid_index = nat

type bonding_curve = nat -> tez

type bonding_curves = (nat, bonding_curve) big_map

type bid_heap_key = 
  [@layout:comb]
  {
    auction_id : auction_id;
    bid_index : bid_index;
  }

type bid_heap = (bid_heap_key, bid_id) big_map

type bid_registry_key = 
  [@layout:comb]
  {
   auction_id : auction_id;
   bid_id : bid_id; 
  }

type bid_param = 
 [@layout:comb]
 {  
    auction_id : nat;
    price : tez;
    quantity : nat;
    is_bid_increase : bid_registry_key option;
 }

type bid = 
  [@layout:comb]
  {
    price : tez;
    bidder : address;
    quantity : nat;
#if OFFCHAIN_BID
    is_offchain : bool;
#endif
    bid_time : timestamp;
    heap_index : bid_index;
  }

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
    price_floor : tez;
    start_time : timestamp;
    last_bid_time : timestamp;
    round_time : int;
    extend_time : int;
    fa2_address : address;
    end_time : timestamp;
    bonding_curve : nat;
    bid_index : nat;
    num_offers_to_payout_or_return : nat; 
    num_winning_offers : nat option;
    winning_price : tez option;
    is_canceled : bool;
    next_token_id : nat option;
    profit_address : address;
    highest_offer_price : tez;
    token_info : (string, bytes) map;
  }

type configure_param =
  [@layout:comb]
  {
    price_floor : tez;
    round_time : nat;
    extend_time : nat;
    fa2_address : address;
    start_time : timestamp;
    end_time : timestamp;
    bonding_curve : nat;
    profit_address : address;
    token_info : (string, bytes) map;
  }

type auction_without_configure_entrypoints =
  | Bid of bid_param
  | Cancel of auction_id
  | Resolve of auction_id
  | Admin of admin_entrypoints
#if OFFCHAIN_BID
  | Offchain_bid of permit_multiunit_bid_param
#endif
  | Return_old_bids of auction_id * nat
  | Return_old_offers of auction_id * nat
  | Payout_winners of auction_id * nat
  | Add_bonding_curve of bonding_curve
 
type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

type bid_registry =  (bid_registry_key, bid) big_map

type heap_sizes = (auction_id, nat) big_map

type storage =
  [@layout:comb]
  {
    admin : admin_storage;
    auction_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map;
    bonding_curve_index : nat;
    bonding_curves : bonding_curves;
    bids : bid_registry;
    bid_heap : bid_heap;
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

let bid_is_less_than(bid1, bid2 : bid * bid) : bool =
  if bid1.price < bid2.price 
  then true 
  else 
       if bid1.price = bid2.price
       then bid1.bid_time > bid2.bid_time
       else false

let swap_heap_keys (i_key, j_key, i_id, j_id, bid_heap, bid_registry : bid_heap_key * bid_heap_key * bid_id * bid_id * bid_heap * bid_registry) 
  : bid_heap * bid_registry =
  let i_replaced_heap : bid_heap = 
      Big_map.update i_key (Some j_id) bid_heap in 
  let ij_replaced_heap : bid_heap = 
      Big_map.update j_key (Some i_id) i_replaced_heap in 
  
  let j_registry_key : bid_registry_key = {auction_id = j_key.auction_id; bid_id = j_id;} in
  let i_registry_key : bid_registry_key = {auction_id = i_key.auction_id; bid_id = i_id;} in

  let j_bid : bid = match (Big_map.find_opt j_registry_key bid_registry) with 
        Some j_bid -> j_bid 
      | None -> (failwith "HEAP_SWAP_FAILS" : bid)
      in 
  let j_replaced_registry : bid_registry = 
    Big_map.update j_registry_key (Some {j_bid with heap_index = i_key.bid_index}) bid_registry 
    in 
  let i_bid : bid = match (Big_map.find_opt i_registry_key bid_registry) with 
      Some i_bid -> i_bid 
    | None -> (failwith "HEAP_SWAP_FAILS" : bid)
    in 
  let ji_replaced_registry : bid_registry = 
    Big_map.update i_registry_key (Some {i_bid with heap_index = j_key.bid_index}) j_replaced_registry 
    in 
  (ij_replaced_heap, ji_replaced_registry)

let get_bid_from_registry (registry_key, bid_registry : bid_registry_key * bid_registry) : bid option = 
  Big_map.find_opt registry_key bid_registry

let get_bid (index_key, bid_heap, bid_registry, error : bid_heap_key * bid_heap * bid_registry * string) : (bid_id * bid) option = 
  let possible_id = Big_map.find_opt index_key bid_heap in 
  let {auction_id = auction_id; bid_index = bid_index;} = index_key in 
  (
    match possible_id with 
        Some id -> 
          let bid_registry_key : bid_registry_key = {auction_id = auction_id; bid_id = id;} in
          let possible_bid : bid option = get_bid_from_registry(bid_registry_key, bid_registry) in 
          (match possible_bid with 
              Some bid -> Some (id, bid)
            | None -> (failwith error : (bid_id * bid) option)
          )
             
      | None -> (None : (bid_id * bid) option)
  )   
    

let get_min (auction_id, bid_heap, bid_registry : auction_id * bid_heap * bid_registry) : nat * bid = 
  let min_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in 
  let possible_bid_info : (bid_id * bid) option = get_bid(min_key, bid_heap, bid_registry, "get_min") in  
  (
    match possible_bid_info with 
        Some bid_info -> bid_info
      | None -> (failwith "INTERNAL_ERROR_MIN" : bid_id * bid) 
  )
(*Maintains min heap for bid inserted at end, swaps with parent bids who have a smaller index*)
let rec maintain_min_heap (bid_heap, bid_registry, current_key, current_id, current_bid : bid_heap * bid_registry * bid_heap_key * bid_id * bid) : bid_heap * bid_registry=
  let parent_key : bid_heap_key = {auction_id = current_key.auction_id; bid_index = parent(current_key.bid_index);} in
  let parent_bid_info_option : (bid_id * bid) option = get_bid(parent_key, bid_heap, bid_registry, "maintain_min_heap") in 
  match parent_bid_info_option with 
      Some parent_bid_info -> 
        let (parent_id, parent_bid) = parent_bid_info in
        (
        if current_key.bid_index = 0n || bid_is_less_than(parent_bid, current_bid)
        then (bid_heap, bid_registry)
        else 
             let (bid_heap, bid_registry) : bid_heap * bid_registry = swap_heap_keys(parent_key, current_key, parent_id, current_id, bid_heap, bid_registry) in 
             maintain_min_heap (bid_heap, bid_registry, parent_key, current_id, current_bid)
        )
    | None -> (bid_heap, bid_registry)

let get_heap_size(auction_id, heap_sizes : auction_id * heap_sizes) : nat = 
    match (Big_map.find_opt auction_id heap_sizes) with 
        Some size -> size 
      | None -> 0n

let update_heap_size(auction_id, heap_sizes, new_size : auction_id * heap_sizes * nat) : heap_sizes = 
    Big_map.update auction_id (Some new_size) heap_sizes
   
let insert_bid (bid, bid_heap, auction_id, insert_index, bid_registry, heap_size : bid * bid_heap * auction_id * nat * bid_registry * nat) : bid_heap * bid_registry =
  (*insert bid at end of heap*)
  let bid_key : bid_registry_key = {auction_id = auction_id; bid_id = insert_index;} in
  let bid_registry : bid_registry = Big_map.add bid_key bid bid_registry in
  let heap_key : bid_heap_key = {auction_id = auction_id; bid_index = heap_size;} in
  let bid_heap : bid_heap = Big_map.add heap_key insert_index bid_heap in 
  (*maintain min heap property*)
  let (bid_heap, bid_registry) : bid_heap * bid_registry= maintain_min_heap (bid_heap, bid_registry, heap_key, insert_index, bid) in 
  (bid_heap, bid_registry)

(*Assumes non-empty heap; Maintains min heap by percolating small bids with low index in direction of their children*)
let rec min_heapify (index_key, bid_heap, heap_size, bid_registry : bid_heap_key * bid_heap * nat * bid_registry) : bid_heap * bid_registry= 
   let index_bid_info_option : (bid_id * bid) option = get_bid(index_key, bid_heap, bid_registry, "min_heapify") in
   let (index_bid_id, index_bid) : (bid_id * bid) = match index_bid_info_option with 
        Some index_bid_info -> index_bid_info
      | None -> (failwith "INTERNAL_ERROR_MIN_HEAPIFY" : bid_id * bid)
   in 
   let (smallest_key, smallest_bid_id, smallest_bid) = (index_key, index_bid_id, index_bid) in

   let l_index : nat = left_child(index_key.bid_index) in 
   let l_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = l_index;} in
   let l_bid_info_option : (bid_id * bid) option = get_bid(l_key, bid_heap, bid_registry, "min heapify L") in 
   
   let (smallest_key, smallest_bid_id, smallest_bid) : bid_heap_key * bid_id * bid= 
     match l_bid_info_option with 
         Some l_bid_info -> 
             let (l_bid_id, l_bid) = l_bid_info in 
             if l_index < heap_size && bid_is_less_than(l_bid, index_bid)
             then (l_key, l_bid_id, l_bid) 
             else (smallest_key, smallest_bid_id, smallest_bid)
       | None -> (smallest_key, smallest_bid_id, smallest_bid)
    in 

   let r_index : nat = right_child(index_key.bid_index) in 
   let r_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = r_index;} in
   let r_bid_info_option : (bid_id * bid) option = get_bid(r_key, bid_heap, bid_registry, "min heapify R") in 

   let (smallest_key, smallest_bid_id, smallest_bid) : bid_heap_key * bid_id * bid = 
     match r_bid_info_option with 
         Some r_bid_info -> 
           let (r_bid_id, r_bid) = r_bid_info in
           if r_index < heap_size && bid_is_less_than(r_bid, smallest_bid)
           then (r_key, r_bid_id, r_bid)
           else (smallest_key, smallest_bid_id, smallest_bid)
      |  None -> (smallest_key, smallest_bid_id, smallest_bid)
    in
   (
     if smallest_key <> index_key 
     then 
          let (bid_heap, bid_registry) : bid_heap * bid_registry = swap_heap_keys(smallest_key, index_key, smallest_bid_id, index_bid_id, bid_heap, bid_registry) in 
          min_heapify(smallest_key, bid_heap, heap_size, bid_registry)
     else 
          (bid_heap, bid_registry)
   )
   
let extract_min (bid_heap, auction_id, heap_size, bid_registry : bid_heap * auction_id * nat * bid_registry) : 
   bid option * bid_heap * nat * bid_registry = 
   ( 
    if heap_size <= 0n 
    then ((None : bid option), bid_heap, heap_size, bid_registry)
    else  
          let new_heap_size : nat = abs(heap_size - 1n) in 
          let percolate_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = new_heap_size;} in
          let percolate_bid_info_option : (bid_id * bid) option 
              = get_bid(percolate_bid_key, bid_heap, bid_registry, "extract min") in
          let (percolate_bid_id, percolate_bid) : (bid_id * bid) =  match percolate_bid_info_option with 
               Some percolate_bid_info -> percolate_bid_info
            |  None -> (failwith "HEAP_GET_FAILS_INTERNAL_ERROR" : bid_id * bid )
            in 
          let percolate_registry_key : bid_registry_key = {auction_id = auction_id; bid_id = percolate_bid_id;} in
          let bid_heap : bid_heap = Big_map.update percolate_bid_key (None : bid_id option) bid_heap in 
         ( 
          if new_heap_size <= 0n
          then ((Some percolate_bid), bid_heap, new_heap_size, bid_registry) (*Case of single bid*)
          else 
              let min_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in
              let min_bid_info_option : (bid_id * bid) option 
                  = get_bid(min_bid_key, bid_heap, bid_registry, "extract min 2nd part") in
              ( match min_bid_info_option with 
                    Some min_bid_info -> 
                        let (min_id, min_bid) = min_bid_info in
                        let min_bid_registry_key : bid_registry_key = {auction_id = auction_id; bid_id = min_id;} in 
                        let bid_heap = Big_map.update min_bid_key (Some percolate_bid_id) bid_heap in 
                        let bid_registry = Big_map.update percolate_registry_key (Some {percolate_bid with heap_index = 0n;}) bid_registry in
                        let bid_registry = Big_map.update min_bid_registry_key (None : bid option) bid_registry in
                        let (bid_heap, bid_registry) : bid_heap * bid_registry
                            = min_heapify(min_bid_key, bid_heap, new_heap_size, bid_registry) in 
                        ((Some min_bid), bid_heap, new_heap_size, bid_registry)
                  | None -> (failwith "HEAP_GET_FAILS_INTERNAL_ERROR" : bid option * bid_heap * nat * bid_registry) (*Case of single bid*)
              )
         )
   )

let delete_bid(key, bid_registry, deleter, heap_size, bid_heap : bid_registry_key * bid_registry * address * nat * bid_heap) : bid_registry * bid_heap * tez * nat = 
   let bid_option : bid option 
     = get_bid_from_registry(key, bid_registry) in
   let bid : bid = match bid_option with 
        Some bid -> bid 
      | None -> (failwith "BID_DOES_NOT_EXIST" : bid)
     in
   let bid_index = bid.heap_index in 
   let bid_heap_key : bid_heap_key = {auction_id = key.auction_id; bid_index = bid_index;} in 
   
   let bid_id_in_heap : bid_id = match (Big_map.find_opt bid_heap_key bid_heap) with 
       Some bid_id -> bid_id 
     | None -> (failwith "NO_BID_ID_STORED_IN_HEAP" : bid_id)
     in 
   let u : unit = assert_msg(bid_id_in_heap = key.bid_id, "BID IDs do not match") in 
   let bid_set_to_min : bid = {bid with price = 0tez} in 
   let bid_price : tez = bid.price in
   let bid_quantity : nat = bid.quantity in 
   
   let bid_registry : bid_registry = 
     if deleter = bid.bidder 
     then 
         let new_registry : bid_registry = Big_map.update key (Some bid_set_to_min) bid_registry in 
         new_registry  
     else (failwith "INVALID_BID_UPDATER" : bid_registry)
     in 
   
   let (bid_heap, bid_registry) 
        = maintain_min_heap(bid_heap, bid_registry, bid_heap_key, key.bid_id, bid_set_to_min) in 
   let (possible_bid, new_min_heap, new_heap_size, new_bid_registry) : bid option * bid_heap * nat * bid_registry
         = extract_min(bid_heap, key.auction_id, heap_size, bid_registry) in 
   let u : unit = (match possible_bid with 
        Some bid -> 
          if bid.price = 0tez
          then ()
          else (failwith "WRONG_BID_DELETED" : unit)
      | None -> (failwith "NO_BID_DELETED" : unit)
   ) in 
   let u : unit = assert_msg(new_heap_size + 1n = heap_size, "EXTRACT_FAILURE") in 
   (new_bid_registry, new_min_heap, bid_price, bid_quantity)

let get_bonding_curve(bc_id, bonding_curve_bm : nat * bonding_curves) : bonding_curve = 
  let bonding_curve : bonding_curve = match (Big_map.find_opt bc_id bonding_curve_bm) with 
      Some bc -> bc 
    | None -> (failwith "INTERNAL_ERROR" : bonding_curve)
  in  
  bonding_curve

let mint_tokens(fa2_address, mint_param : address * mint_tokens_param) : operation = 
  let c = address_to_contract_mint_entrypoint(fa2_address) in
  let op : operation = Tezos.transaction mint_param 0mutez c in
  op

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

let bid_amount_sent (bid_param: bid_param) : bool =
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

    assert_msg (configure_param.price_floor > 0mutez, "INVALID_PRICE_FLOOR");
    tez_stuck_guard("CONFIGURE");
    assert_msg (configure_param.round_time > 0n, "INVALID_ROUND_TIME");

    assert_msg(Big_map.mem configure_param.bonding_curve storage.bonding_curves, "INVALID_BONDING_CURVE");

    let auction_data : auction = {
      seller = seller;
      price_floor = configure_param.price_floor;
      start_time = configure_param.start_time;
      round_time = int(configure_param.round_time);
      extend_time = int(configure_param.extend_time);
      fa2_address = configure_param.fa2_address;
      end_time = configure_param.end_time;
      last_bid_time = configure_param.start_time;
      bonding_curve = configure_param.bonding_curve;
      bid_index = 0n;
      winning_price = (None : tez option);
      num_winning_offers = (None : nat option);
      num_offers_to_payout_or_return = 0n;
      highest_offer_price = 0tez;
      is_canceled = false;
      next_token_id = (None : nat option);
      profit_address = configure_param.profit_address;
      token_info = configure_param.token_info;
    } in
    let updated_auctions : (nat, auction) big_map = Big_map.update storage.auction_id (Some auction_data) storage.auctions in
    {storage with auctions = updated_auctions; auction_id = storage.auction_id + 1n}
  end

let configure_auction(configure_param, storage : configure_param * storage) : return =
  let new_storage = configure_auction_storage(configure_param, Tezos.sender, storage) in
  (([] : operation list), new_storage)

(*Sets strike price iff all invalid bids and offers are returned, and sends out profit and reserve amounts*)
let resolve_auction(auction_id, storage : nat * storage) : return = begin
  (fail_if_paused storage.admin);
  tez_stuck_guard("RESOLVE");
  let auction : auction = get_auction_data(auction_id, storage) in
  assert_msg (auction_ended(auction) , "AUCTION_NOT_ENDED");
  assert_msg (not auction.is_canceled, "AUCTION_CANCELED");
  let auction_resolved : bool = match auction.winning_price with 
      Some wp -> true 
    | None -> false 
    in  
  assert_msg(not auction_resolved, "AUCTION_ALREADY_RESOLVED");
  
  (
    if auction.num_offers_to_payout_or_return = 0n 
    then 
        let updated_auction_data : auction = {auction with winning_price = Some 0mutez; num_winning_offers = Some 0n;} in (*WHY SETTING PRICE TO 0MUTEZ?*)
        let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
        (([] : operation list), {storage with auctions = updated_auctions;})
    else 
        let (_, min_bid) : nat * bid = get_min(auction_id, storage.bid_heap, storage.bids) in 
        let min_price : tez = min_bid.price in 
        let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
        let min_price_valid_at_q : tez = bonding_curve auction.num_offers_to_payout_or_return in 
        let u : unit = assert_msg(min_price >= min_price_valid_at_q, "NOT_ALL_INVALID_BIDS_AND_OFFERS_RETURNED") in 
        let winning_price : tez = min_price_valid_at_q in
        let num_winning_offers : nat = auction.num_offers_to_payout_or_return in  
        let total_fee : tez = (winning_price * num_winning_offers) in 
        let transfer_fee_op : operation = transfer_tez(total_fee, auction.profit_address) in 
        let op_list : operation list = [transfer_fee_op] in
        let updated_auction_data : auction = {auction with winning_price = Some winning_price; num_winning_offers = Some num_winning_offers; next_token_id = Some num_winning_offers;} in
        let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
        (op_list , {storage with auctions = updated_auctions;})  
  )
 end

let cancel_auction(auction_id, storage : nat * storage) : return = begin
    let auction : auction = get_auction_data(auction_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
      else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    tez_stuck_guard("CANCEL");
    let updated_auction_data : auction = {auction with is_canceled = true;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    (([] : operation list) , {storage with auctions = updated_auctions;})  
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
    let current_heap_size : nat = get_heap_size(bid_param.auction_id, storage.heap_sizes) in
    let (bid_registry, bid_heap, necessary_fee, current_heap_size, quantity_diff) :  bid_registry * bid_heap * tez  * nat * nat= 
      (match bid_param.is_bid_increase with
          | Some bid_registry_key -> begin
             let (bid_registry, bid_heap, bid_amount, bid_quantity) = delete_bid(bid_registry_key, storage.bids, bidder, current_heap_size, storage.bid_heap) in 
             assert_msg(bid_param.quantity >= bid_quantity && bid_param.price >= bid_amount, "BID_MUST_INCREASE");
             let fee_needed : tez = (bid_param.quantity * bid_param.price) - (bid_amount * bid_quantity) in 
             (bid_registry, bid_heap, fee_needed, abs(current_heap_size - 1n), abs(bid_param.quantity - bid_quantity))
            end
          | None ->
             (storage.bids, storage.bid_heap, bid_param.price * bid_param.quantity, current_heap_size, bid_param.quantity)
      ) in

    (if (Tezos.amount <> necessary_fee
#if OFFCHAIN_BID 
        && not is_offchain
#endif
        ) || bid_param.price < auction.price_floor 
      then ([%Michelson ({| { FAILWITH } |} : string * (address * tez * timestamp * timestamp) -> unit)] ("INVALID_BID_AMOUNT", (bidder, bid_param.price, auction.last_bid_time, Tezos.now)) : unit)
      else ());
  
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let insert_index = auction.bid_index in
    let new_bid : bid = 
      ({
        quantity = bid_param.quantity;
        price = bid_param.price;
        bidder = bidder;
#if OFFCHAIN_BID
        is_offchain = is_offchain;
#endif
        bid_time = Tezos.now;
        heap_index = current_heap_size;
      } : bid) in 

    let (new_bid_heap, new_bid_registry) : bid_heap * bid_registry = 
        insert_bid(new_bid, bid_heap, bid_param.auction_id, insert_index, bid_registry, current_heap_size) in

    let updated_num_offers : nat = auction.num_offers_to_payout_or_return + quantity_diff in 
    
    let new_heap_size : nat = current_heap_size + 1n in 
    let new_heap_size_bm : heap_sizes = update_heap_size(bid_param.auction_id, storage.heap_sizes, new_heap_size) in 

    let new_highest_offer_price : tez = 
      if bid_param.price <= auction.highest_offer_price 
      then auction.highest_offer_price
      else bid_param.price 
      in  

    let updated_auction_data : auction = {auction with last_bid_time = Tezos.now; end_time = new_end_time; 
                                bid_index = auction.bid_index + 1n; 
                                num_offers_to_payout_or_return = updated_num_offers;
                                highest_offer_price = new_highest_offer_price } in
    let updated_auctions = Big_map.update bid_param.auction_id (Some updated_auction_data) storage.auctions in
    (([] : operation list) , {storage with auctions = updated_auctions; bids = new_bid_registry; bid_heap = new_bid_heap; heap_sizes = new_heap_size_bm;})
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

let rec num_valid_offers_remaining_after_returning_max_n_offers(num_offers_remaining, bid_price, bonding_curve, auction_is_canceled, offers_to_return : nat * tez * bonding_curve * bool * int) 
  : nat = 

  if num_offers_remaining = 0n || offers_to_return <= 0
  then num_offers_remaining
  else 
    let min_price_valid_at_old_Q : tez = bonding_curve num_offers_remaining in 
    let offer_returnable : bool =  bid_price < min_price_valid_at_old_Q || auction_is_canceled in
    if offer_returnable 
    then 
         num_valid_offers_remaining_after_returning_max_n_offers( abs(num_offers_remaining - 1n), bid_price, bonding_curve, auction_is_canceled, offers_to_return - 1)
    else 
         num_offers_remaining


(*Claim is that after returning these offers, either we can still return more or property is satisfied and returning 1 less would have made it unsatisfied*)
let return_offers(auction_id, num_offers_to_return, storage : auction_id * nat * storage) : return = begin 
    tez_stuck_guard("RETURN_OLD_OFFERS");
    (fail_if_paused storage.admin); 
    let auction : auction = get_auction_data(auction_id, storage) in
    let u : unit = match auction.winning_price with 
        Some wp -> (failwith "AUCTION_ALREADY_RESOLVED" : unit)
      | None -> ()
      in  
    let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_OFFERS_LEFT");
    let (min_id, min_bid) : nat * bid = get_min(auction_id, storage.bid_heap, storage.bids) in 
    let bid_price : tez = min_bid.price in 
    let bid_quantity : nat = min_bid.quantity in
    let new_total_offers : nat = num_valid_offers_remaining_after_returning_max_n_offers(bid_quantity, bid_price, bonding_curve, auction.is_canceled, int(num_offers_to_return)) in
    
    (if bid_quantity = new_total_offers 
    then (([] : operation list), storage)
    else 
      let op_list : operation list = 
#if OFFCHAIN_BID
        if min_bid.is_offchain 
        then ([] : operation list)
        else 
#endif   
          let return_amt : tez = abs(bid_quantity - new_total_offers) * min_bid.price in 
          let offers_return_op : operation = transfer_tez(return_amt, min_bid.bidder) in (*Returns difference of bid and winning_price*)
          [offers_return_op] 
        in
      let updated_auction_data : auction = {auction with num_offers_to_payout_or_return = new_total_offers;} in
      let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
      let bid_key : bid_registry_key = {auction_id = auction_id; bid_id = min_id;} in
      let bid_registry : bid_registry = Big_map.update bid_key (Some {min_bid with quantity = new_total_offers;}) storage.bids in 
      (op_list, {storage with auctions = updated_auctions; bids = bid_registry;})
    )
  end

let rec return_invalid_bids(bid_heap, bid_registry, num_offers_to_payout_or_return, bonding_curve, auction_id, auction_is_canceled, heap_size, bids_to_return, price_floor, op_list : bid_heap * bid_registry * nat * bonding_curve * auction_id * bool * nat * int * tez * operation list) 
  : bid_heap * bid_registry * nat * nat * tez * operation list= 

  if num_offers_to_payout_or_return = 0n || bids_to_return <= 0
  then (bid_heap, bid_registry, heap_size, num_offers_to_payout_or_return, price_floor, op_list)
  else 
    let (possible_min_bid, new_bid_heap, new_heap_size, new_bid_registry) = extract_min(bid_heap, auction_id, heap_size, bid_registry) in
    match possible_min_bid with 
        Some bid -> 
          let min_bid_price : tez = bid.price in 
          let min_price_valid_at_old_Q : tez = bonding_curve num_offers_to_payout_or_return in
          let remaining_offers : nat = abs(num_offers_to_payout_or_return - bid.quantity) in 
          let min_price_valid_at_new_Q_plus_one : tez = bonding_curve (remaining_offers + 1n) in 
          let bid_returnable : bool =  (min_bid_price < min_price_valid_at_old_Q  && min_bid_price < min_price_valid_at_new_Q_plus_one)|| auction_is_canceled in
          if bid_returnable 
          then 
            let new_op_list = 
#if OFFCHAIN_BID
              if bid.is_offchain 
              then op_list 
              else 
#endif   
                let return_amt : tez = bid.quantity * bid.price in 
                let bid_return_op : operation = transfer_tez(return_amt, bid.bidder) in 
                bid_return_op :: op_list 
                in
            return_invalid_bids(new_bid_heap, new_bid_registry, remaining_offers, bonding_curve, auction_id, auction_is_canceled, new_heap_size, bids_to_return - 1, min_bid_price, new_op_list)
          else 
               (bid_heap, bid_registry, heap_size, num_offers_to_payout_or_return, price_floor, op_list)
      | None -> (failwith "INTERNAL_ERROR_BID_RETURN" : bid_heap * bid_registry * nat * nat * tez * operation list)

let empty_heap(auction_id, num_bids_to_return, storage : auction_id * nat * storage) : return = begin
    tez_stuck_guard("RETURN_OLD_BIDS");
    (fail_if_paused storage.admin);
    let num_bids_to_return : int = int(num_bids_to_return) in (*Cast to int for recursion, decrementing by 1 each step*)
    let auction : auction = get_auction_data(auction_id, storage) in
    let u : unit = match auction.winning_price with 
        Some wp -> (failwith "AUCTION_ALREADY_RESOLVED" : unit)
      | None -> ()
      in  
    let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_BIDS_LEFT");
    let (bid_heap, bid_registry, new_heap_size, num_offers_to_payout_or_return, price_floor, op_list) = 
        return_invalid_bids(storage.bid_heap, storage.bids, auction.num_offers_to_payout_or_return, bonding_curve, auction_id, auction.is_canceled, heap_size, num_bids_to_return, auction.price_floor, ([] : operation list)) in 
    let new_heap_size_bm : heap_sizes = update_heap_size(auction_id, storage.heap_sizes, new_heap_size) in 
    let updated_auction_data : auction = {auction with num_offers_to_payout_or_return = num_offers_to_payout_or_return; price_floor = price_floor;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    (op_list , {storage with auctions = updated_auctions; bids = bid_registry; bid_heap = bid_heap; heap_sizes = new_heap_size_bm;})
  end  

let rec mint_n_tokens_to_owner(mint_param, owner, next_token_id, token_info, num_to_mint : mint_tokens_param * address * nat * token_info * int) : mint_tokens_param * nat = 
  if num_to_mint <= 0 
  then (mint_param, next_token_id)
  else 
       let mint_token : mint_token_param = {
         owner = owner;
         token_metadata = {
          token_id = next_token_id;
          token_info = token_info;
         };
       } in
       let mint_param = mint_token :: mint_param in
       mint_n_tokens_to_owner(mint_param, owner, abs(next_token_id - 1n), token_info, num_to_mint - 1)

let rec pay_winning_bids(op_list, bid_heap, mint_param, auction_id, heap_size, winners_to_payout, winning_price, num_offers_to_payout_or_return, next_token_id, token_info, bid_registry : 
      operation list * bid_heap * mint_tokens_param * auction_id * nat * int * tez * nat * nat * token_info * bid_registry)
    : operation list * bid_heap * mint_tokens_param * nat * nat * nat * bid_registry = 
  if winners_to_payout > 0
  then 
       let (possible_bid, bid_heap, heap_size, bid_registry) = extract_min(bid_heap, auction_id, heap_size, bid_registry) in 
       match possible_bid with 
           Some bid -> 
             let (mint_param, next_token_id) = mint_n_tokens_to_owner(mint_param, bid.bidder, next_token_id, token_info, int(bid.quantity)) in  
             let remaining_offers : nat = abs(num_offers_to_payout_or_return - bid.quantity) in 
             let op_list = 
#if OFFCHAIN_BID
                 if bid.is_offchain 
                 then op_list 
                 else 
#endif   
                      if bid.price = winning_price
                      then op_list
                      else 
                        let return_amt : tez = bid.quantity * (bid.price - winning_price) in 
                        let bid_return_op : operation = transfer_tez(return_amt, bid.bidder) in (*Returns difference of bid and winning_price*)
                        bid_return_op :: op_list in
             pay_winning_bids(op_list, bid_heap, mint_param, auction_id, heap_size, winners_to_payout - 1, winning_price, remaining_offers, next_token_id, token_info, bid_registry)
         | None -> (op_list, bid_heap, mint_param, heap_size, num_offers_to_payout_or_return, next_token_id, bid_registry) (*This should never be reached, get_min will fail*)
  else 
       (op_list, bid_heap, mint_param, heap_size, num_offers_to_payout_or_return, next_token_id, bid_registry)

let payout(auction_id, num_winners_to_payout, storage : auction_id * nat * storage) : return = begin
    tez_stuck_guard("PAYOUT");
    (fail_if_paused storage.admin);
    let num_winners_to_payout : int = int(num_winners_to_payout) in (*Cast to int for recursion, decrementing by 1 each step*)
    let auction : auction = get_auction_data(auction_id, storage) in 
    let winning_price : tez = match auction.winning_price with 
        Some wp -> wp
      | None -> (failwith "AUCTION_NOT_RESOLVED" : tez)
      in   
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_WINNERS_LEFT");
    let initial_token_id : nat = match auction.next_token_id with 
          Some ntid -> ntid
        | None -> (failwith "INTERNAL_ERROR" : nat)
        in
    let (op_list, bid_heap, mint_param, new_heap_size, num_offers_to_payout_or_return, next_token_id, bid_registry) = 
        pay_winning_bids(([] : operation list), storage.bid_heap, ([] : mint_tokens_param), auction_id, heap_size, num_winners_to_payout, winning_price, auction.num_offers_to_payout_or_return, initial_token_id, auction.token_info, storage.bids) in 
    let new_heap_size_bm : heap_sizes = update_heap_size(auction_id, storage.heap_sizes, new_heap_size) in 
    let updated_auction_data : auction = {auction with num_offers_to_payout_or_return = num_offers_to_payout_or_return; next_token_id = Some next_token_id;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    let mint_tx : operation = mint_tokens(auction.fa2_address, mint_param) in 
    (mint_tx :: op_list, {storage with auctions = updated_auctions; bids = bid_registry; heap_sizes = new_heap_size_bm; bid_heap = bid_heap;})
  end

let multiunit_bonding_curve_auction_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid bid_param -> place_bid_onchain(bid_param, storage)
    | Cancel auction_id -> cancel_auction(auction_id, storage)
    | Resolve auction_id -> resolve_auction(auction_id, storage)
    | Admin a -> 
        let (ops, admin_storage) = admin_main(a, storage.admin) in
        (ops, { storage with admin = admin_storage })
#if OFFCHAIN_BID
    | Offchain_bid permit -> bid_with_permit(permit, storage)
#endif
    | Return_old_bids return_bids_param -> 
        let (auction_id, num_bids_to_return) = return_bids_param in 
        empty_heap(auction_id, num_bids_to_return, storage)
    | Return_old_offers return_offers_param -> (*return offers of lowest bid*)
        let (auction_id, num_offers_to_return) = return_offers_param in 
        return_offers(auction_id, num_offers_to_return, storage) 
    | Payout_winners payout_param ->
        let (auction_id, num_winners_to_payout) = payout_param in 
        payout(auction_id, num_winners_to_payout, storage)
    | Add_bonding_curve bc -> 
        let new_bonding_curve_bm : bonding_curves = Big_map.add storage.bonding_curve_index bc storage.bonding_curves in 
        (([] : operation list), {storage with bonding_curves = new_bonding_curve_bm;
                                  bonding_curve_index = storage.bonding_curve_index + 1n})
         

let multiunit_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> multiunit_bonding_curve_auction_no_configure(ai, storage)
