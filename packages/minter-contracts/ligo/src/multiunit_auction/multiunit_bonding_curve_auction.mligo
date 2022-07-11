#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../minter_collection/nft/fa2_multi_nft_manager.mligo"
#include "../common.mligo"

type auction_id = nat

type bonding_curve = nat -> tez

type bonding_curves = (nat, bonding_curve) big_map

type bid_param = 
 [@layout:comb]
 {  
    auction_id : nat;
    price : tez;
    quantity : nat;
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
    num_offers : nat; 
    winning_price : tez option;
    is_canceled : bool;
    next_token_id : nat;
    reserve_address : address;
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
    initial_token_id : nat;
    reserve_address : address;
    profit_address : address;
    token_info : (string, bytes) map;
  }

type auction_without_configure_entrypoints =
  | Bid of bid_param
  | Cancel of auction_id
  | Resolve of auction_id
  | Admin of pauseable_admin
#if OFFCHAIN_BID
  | Offchain_bid of permit_multiunit_bid_param
#endif
  | Return_old_bids of auction_id * nat
  | Return_old_offers of auction_id * nat
  | Payout_winners of auction_id * nat
  | Add_bonding_curve of bonding_curve * bonding_curve

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
    bonding_curves : bonding_curves;
    bonding_curve_integrals : bonding_curves;
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

let bid_is_less_than(bid1, bid2 : bid * bid) : bool =
  if bid1.price < bid2.price 
  then true 
  else 
       if bid1.price = bid2.price
       then bid1.bid_time < bid2.bid_time
       else false

let swap_heap_keys (i_key, j_key, i_bid, j_bid, bid_heap : bid_heap_key * bid_heap_key * bid * bid * bid_heap) : bid_heap =
  let i_replaced_heap : bid_heap = 
      Big_map.update i_key (Some j_bid) bid_heap in 
  let ij_replaced_heap : bid_heap = 
      Big_map.update j_key (Some i_bid) i_replaced_heap in 
  ij_replaced_heap

let get_bid (index_key, bid_heap : bid_heap_key * bid_heap) : bid option = 
  Big_map.find_opt index_key bid_heap

let get_min (auction_id, bid_heap : auction_id * bid_heap) : bid = 
  let min_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in 
  let possible_min : bid option = get_bid(min_key, bid_heap) in 
  match possible_min with 
      Some min -> min 
    | None -> (failwith "NO_BIDS" : bid) 

let rec maintain_min_heap (bid_heap, current_key, current_bid : bid_heap * bid_heap_key * bid) : bid_heap =
  let parent_key : bid_heap_key = {auction_id = current_key.auction_id; bid_index = parent(current_key.bid_index);} in
  let parent_bid_option : bid option = get_bid(parent_key, bid_heap) in 
  match parent_bid_option with 
      Some parent_bid -> 
        (
        if current_key.bid_index = 0n || parent_bid.price <= current_bid.price
        then bid_heap
        else 
             let bid_heap : bid_heap = swap_heap_keys(parent_key, current_key, parent_bid, current_bid, bid_heap) in 
             maintain_min_heap (bid_heap, parent_key, parent_bid)
        )
    | None -> bid_heap

let get_heap_size(auction_id, heap_sizes : auction_id * heap_sizes) : nat = 
    match (Big_map.find_opt auction_id heap_sizes) with 
        Some size -> size 
      | None -> 0n

let update_heap_size(auction_id, heap_sizes, new_size : auction_id * heap_sizes * nat) : heap_sizes = 
    Big_map.update auction_id (Some new_size) heap_sizes
   
let insert_bid (bid, bid_heap, auction_id, insert_index : bid * bid_heap * auction_id * nat) : bid_heap =
  (*insert bid at end of heap*)
  let bid_key : bid_heap_key = {auction_id = auction_id; bid_index = insert_index;} in
  let bid_heap : bid_heap = Big_map.add bid_key bid bid_heap in 

  (*maintain min heap property*)
  let bid_heap : bid_heap = maintain_min_heap (bid_heap, bid_key, bid) in 
  bid_heap

(*Assumes non-empty heap*)
let rec min_heapify (index_key, bid_heap, heap_size : bid_heap_key * bid_heap * nat) : bid_heap = 
   let index_bid_option : bid option = get_bid(index_key, bid_heap) in
   let index_bid : bid = match index_bid_option with 
        Some bid -> bid 
      | None -> (failwith "INTERNAL_ERROR" : bid)
   in 
   let (smallest_key, smallest_bid) = (index_key, index_bid) in

   let l_index : nat = left_child(index_key.bid_index) in 
   let l_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = l_index;} in
   let l_bid_option : bid option = get_bid(l_key, bid_heap) in 
   
   let (smallest_key, smallest_bid) : bid_heap_key * bid = 
     match l_bid_option with 
         Some l_bid -> 
             if l_index < heap_size && bid_is_less_than(l_bid, index_bid)
             then (l_key, l_bid) 
             else (smallest_key, smallest_bid)
       | None -> (smallest_key, smallest_bid)
    in 

   let r_index : nat = right_child(index_key.bid_index) in 
   let r_key : bid_heap_key =  {auction_id = index_key.auction_id; bid_index = r_index;} in
   let r_bid_option : bid option = get_bid(r_key, bid_heap) in 

   let (smallest_key, smallest_bid) : bid_heap_key * bid = 
     match r_bid_option with 
         Some r_bid -> 
           if r_index < heap_size && bid_is_less_than(r_bid, smallest_bid)
           then (r_key, r_bid)
           else (smallest_key, smallest_bid)
      |  None -> (smallest_key, smallest_bid)
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
          let percolate_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = new_heap_size;} in
      
          let (percolate_bid_option, bid_heap) : bid option * bid_heap = 
               Big_map.get_and_update percolate_bid_key (None : bid option) bid_heap in 
          let percolate_bid : bid =  match percolate_bid_option with 
               Some bid -> bid 
            |  None -> (failwith "HEAP_GET_FAILS_INTERNAL_ERROR" : bid )
            in 
          
          if new_heap_size <= 0n
          then ((Some percolate_bid), bid_heap, new_heap_size) (*Case of single bid*)
          else 
              let min_bid_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in    
              let (min_bid_option, bid_heap) : bid option * bid_heap = 
                 Big_map.get_and_update min_bid_key (Some percolate_bid) bid_heap in 
              ( match min_bid_option with 
                    Some min_bid -> 
                      let new_heap : bid_heap = min_heapify(min_bid_key, bid_heap, new_heap_size) in 
                      ((Some min_bid), new_heap, new_heap_size)
                  | None -> (failwith "HEAP_GET_FAILS_INTERNAL_ERROR" : bid option * bid_heap * nat ) (*Case of single bid*)
              )
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
      num_offers = 0n;
      highest_offer_price = 0tez;
      is_canceled = false;
      next_token_id = configure_param.initial_token_id;
      reserve_address = configure_param.reserve_address;
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
    if auction.num_offers = 0n 
    then 
        let updated_auction_data : auction = {auction with winning_price = Some 0mutez;} in 
        let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
        (([] : operation list), {storage with auctions = updated_auctions;})
    else 
        let min_bid : bid = get_min(auction_id, storage.bids) in 
        let min_price : tez = min_bid.price in 
        let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
        let min_price_valid_at_q : tez = bonding_curve auction.num_offers in 
        let u : unit = assert_msg(min_price_valid_at_q <= min_price, "NOT_ALL_INVALID_BIDS_AND_OFFERS_RETURNED") in 
        let winning_price : tez = min_price_valid_at_q in 
        let bonding_curve_integral : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curve_integrals) in
        let total_fee : tez = (winning_price * auction.num_offers) in 
        let integral_value : tez = (bonding_curve_integral auction.num_offers) - (bonding_curve_integral 0n) in (*Function must be increasing*)
        let reserve_amount : tez = 
          if integral_value + auction.highest_offer_price > total_fee 
          then total_fee 
          else integral_value + auction.highest_offer_price
          in
        let transfer_reserve_op : operation = transfer_tez(reserve_amount, auction.reserve_address) in 

        let op_list : operation list =  
          if total_fee > reserve_amount 
          then 
            let auction_profit : tez = total_fee - reserve_amount in 
            let transfer_profit_op : operation = transfer_tez(auction_profit, auction.profit_address) in 
            [transfer_reserve_op; transfer_profit_op] 
          else 
            [transfer_reserve_op]
          in 
        let updated_auction_data : auction = {auction with winning_price = Some winning_price;} in
        let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
        (op_list , {storage with auctions = updated_auctions;})  
  )
 end

let cancel_auction(auction_id, storage : nat * storage) : return = begin
    let auction : auction = get_auction_data(auction_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
      else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    assert_msg (not auction_ended(auction), "AUCTION_ENDED");
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
    (if (not bid_amount_sent(bid_param) 
#if OFFCHAIN_BID 
        && not is_offchain
#endif
        ) || bid_param.price < auction.price_floor 
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
        bid_time = Tezos.now;
      } : bid) in 

    let current_heap_size : nat = get_heap_size(bid_param.auction_id, storage.heap_sizes) in 

    let bid_heap : bid_heap = insert_bid(new_bid, storage.bids, bid_param.auction_id, current_heap_size) in

    let updated_num_offers : nat = auction.num_offers + new_bid.quantity in 
    
    let new_heap_size : nat = current_heap_size + 1n in 
    let new_heap_size_bm : heap_sizes = update_heap_size(bid_param.auction_id, storage.heap_sizes, new_heap_size) in 

    let new_highest_offer_price : tez = 
      if bid_param.price <= auction.highest_offer_price 
      then auction.highest_offer_price
      else bid_param.price 
      in  

    let updated_auction_data : auction = {auction with last_bid_time = Tezos.now; end_time = new_end_time; 
                                bid_index = auction.bid_index + 1n; 
                                num_offers = updated_num_offers;
                                highest_offer_price = new_highest_offer_price } in
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

(*Claim is that after returning these offers, either we can still return more or property is satisfied and returning 1 less would have made it unsatisfied*)
let return_offers(auction_id, num_offers_to_return, storage : auction_id * nat * storage) : return = begin 
    tez_stuck_guard("RETURN_OLD_OFFERS");
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(auction_id, storage) in
    let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_BIDS_LEFT");
    let min_bid : bid = get_min(auction_id, storage.bids) in 
    let bid_price : tez = min_bid.price in 
    assert_msg(min_bid.quantity > num_offers_to_return, "RETURN_AMOUNT_TOO_HIGH"); (*If equal to bid quantity, call return bid entrypoint*)
    let min_price_valid_at_Q : tez = bonding_curve auction.num_offers in (*Let Q = total_offers including bid offers*)
    assert_msg(min_bid.price < min_price_valid_at_Q, "NO_OFFERS_CAN_BE_RETURNED"); 
    let new_total_offers : nat = abs(auction.num_offers - num_offers_to_return) in 
    let min_price_valid_at_q : tez = bonding_curve new_total_offers in
    let min_price_valid_at_q_plus_one : tez = bonding_curve (new_total_offers + 1n) in 
    let valid_return_amt : bool = min_bid.price < min_price_valid_at_q || (min_bid.price >= min_price_valid_at_q && min_bid.price < min_price_valid_at_q_plus_one) in
    assert_msg(valid_return_amt, "INVALID_RETURN_AMOUNT");
    let updated_auction_data : auction = {auction with num_offers = new_total_offers;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    let bid_key : bid_heap_key = {auction_id = auction_id; bid_index = 0n;} in
    let bid_heap : bid_heap = Big_map.update bid_key (Some {min_bid with quantity = new_total_offers;}) storage.bids in 
    (([] : operation list), {storage with auctions = updated_auctions; bids = bid_heap;})
  end

let rec return_invalid_bids(bid_heap, num_offers, bonding_curve, auction_id, auction_is_canceled, heap_size, bids_to_return, price_floor : bid_heap * nat * bonding_curve * auction_id * bool * nat * int * tez) 
  : bid_heap * nat * nat * tez = 

  if num_offers = 0n
  then (bid_heap, heap_size, num_offers, price_floor)
  else 
    let min_bid : bid = get_min(auction_id, bid_heap) in 
    let bid_price : tez = min_bid.price in 
    let min_price_valid_at_Q : tez = bonding_curve num_offers in
    let remaining_offers : nat = abs(num_offers - min_bid.quantity) in 
    let min_price_valid_at_q_plus_one : tez = bonding_curve (remaining_offers + 1n) in 
    let bid_returnable : bool =  ((bid_price < min_price_valid_at_Q  && bid_price < min_price_valid_at_q_plus_one)|| auction_is_canceled) && bids_to_return > 0 in
    if bid_returnable 
    then 
         let (possible_bid, bid_heap, heap_size) = extract_min(bid_heap, auction_id, heap_size) in 
         match possible_bid with 
             Some bid -> 
               let price_floor : tez = bid.price in 
               return_invalid_bids(bid_heap, remaining_offers, bonding_curve, auction_id, auction_is_canceled, heap_size, bids_to_return - 1, price_floor)
           | None -> (bid_heap, heap_size, num_offers, price_floor) (*This should not be reached*)
    else 
         (bid_heap, heap_size, num_offers, price_floor)

let empty_heap(auction_id, num_bids_to_return, storage : auction_id * nat * storage) : return = begin
    tez_stuck_guard("RETURN_OLD_BIDS");
    (fail_if_paused storage.admin);
    let num_bids_to_return : int = int(num_bids_to_return) in (*Cast to int for recursion, decrementing by 1 each step*)
    let auction : auction = get_auction_data(auction_id, storage) in
    let bonding_curve : bonding_curve = get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_BIDS_LEFT");
    let (bid_heap, new_heap_size, num_offers, price_floor) = 
        return_invalid_bids(storage.bids, auction.num_offers, bonding_curve, auction_id, auction.is_canceled, heap_size, num_bids_to_return, auction.price_floor) in 
    let new_heap_size_bm : heap_sizes = update_heap_size(auction_id, storage.heap_sizes, new_heap_size) in 
    let updated_auction_data : auction = {auction with num_offers = num_offers; price_floor = price_floor;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    (([] : operation list) , {storage with auctions = updated_auctions; bids = bid_heap; heap_sizes = new_heap_size_bm;})
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
       mint_n_tokens_to_owner(mint_param, owner, next_token_id + 1n, token_info, num_to_mint - 1)

let rec pay_winning_bids(bid_heap, op_list, mint_param, auction_id, heap_size, winners_to_payout, winning_price, num_offers, next_token_id, token_info : bid_heap * operation list * mint_tokens_param * auction_id * nat * int * tez * nat * nat * token_info)
    : bid_heap * operation list * mint_tokens_param * nat * nat * nat= 
  if winners_to_payout > 0
  then 
       let (possible_bid, bid_heap, heap_size) = extract_min(bid_heap, auction_id, heap_size) in 
       match possible_bid with 
           Some bid -> 
             let (mint_param, next_token_id) = mint_n_tokens_to_owner(mint_param, bid.bidder, next_token_id, token_info, int(bid.quantity)) in  
             let op_list = 
#if OFFCHAIN_BID
                 if bid.is_offchain 
                 then op_list 
                 else 
#endif   
                      let return_amt : tez = bid.quantity * (bid.price - winning_price) in (*price ALWAYS will be greater than winning_price, asserted in resolve*)
                      let bid_return_op : operation = transfer_tez(return_amt, bid.bidder) in (*Returns difference of bid and winning_price*)
                      bid_return_op :: op_list in
             let remaining_offers : nat = abs(num_offers - bid.quantity) in 
             pay_winning_bids(bid_heap, op_list, mint_param, auction_id, heap_size, winners_to_payout - 1, winning_price, remaining_offers, next_token_id, token_info)
         | None -> (bid_heap, op_list, mint_param, heap_size, num_offers, next_token_id) (*This should never be reached, get_min will fail*)
  else 
       (bid_heap, op_list, mint_param, heap_size, num_offers, next_token_id)

let payout(auction_id, num_winners_to_payout, storage : auction_id * nat * storage) : return = begin
    tez_stuck_guard("RETURN_OLD_BIDS");
    (fail_if_paused storage.admin);
    let num_winners_to_payout : int = int(num_winners_to_payout) in (*Cast to int for recursion, decrementing by 1 each step*)
    let auction : auction = get_auction_data(auction_id, storage) in 
    let winning_price : tez = match auction.winning_price with 
        Some wp -> wp
      | None -> (failwith "AUCTION_NOT_RESOLVED" : tez)
      in   
    let heap_size : nat = get_heap_size(auction_id, storage.heap_sizes) in
    assert_msg(heap_size > 0n, "NO_WINNERS_LEFT");
    let (bid_heap, op_list, mint_param, new_heap_size, num_offers, next_token_id) = 
        pay_winning_bids(storage.bids, ([] : operation list), ([] : mint_tokens_param), auction_id, heap_size, num_winners_to_payout, winning_price, auction.num_offers, auction.next_token_id, auction.token_info) in 
    let new_heap_size_bm : heap_sizes = update_heap_size(auction_id, storage.heap_sizes, new_heap_size) in 
    let updated_auction_data : auction = {auction with num_offers = num_offers; next_token_id = next_token_id;} in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    let mint_tx : operation = mint_tokens(auction.fa2_address, mint_param) in 
    (mint_tx :: op_list, {storage with auctions = updated_auctions; bids = bid_heap; heap_sizes = new_heap_size_bm;})
  end

[@view]
let get_auction_results(auction_id, storage : nat * storage) : bonding_curve * nat = begin
    let auction : auction = get_auction_data(auction_id, storage) in 
    let bonding_curve : bonding_curve =  get_bonding_curve(auction.bonding_curve, storage.bonding_curves) in 
    match auction.winning_price with 
        Some wp ->  (bonding_curve, auction.num_offers) (*Auction has ended*)
      | None -> (failwith "AUCTION_NOT_ENDED" : bonding_curve * nat)  
  end

let multiunit_bonding_curve_auction_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid bid_param -> place_bid_onchain(bid_param, storage)
    | Cancel auction_id -> cancel_auction(auction_id, storage)
    | Resolve auction_id -> resolve_auction(auction_id, storage)
    | Admin a -> admin(a, storage)
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
    | Add_bonding_curve bc_param -> 
        let (bc, bc_integral) = bc_param in 
        let new_bonding_curve_bm : bonding_curves = Big_map.add storage.bonding_curve_index bc storage.bonding_curves in 
        let new_bonding_curve_integral_bm : bonding_curves = Big_map.add storage.bonding_curve_index bc_integral storage.bonding_curve_integrals in
        (([] : operation list), {storage with bonding_curves = new_bonding_curve_bm; bonding_curve_integrals = new_bonding_curve_integral_bm;
                                   bonding_curve_index = storage.bonding_curve_index + 1n})

let multiunit_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> multiunit_bonding_curve_auction_no_configure(ai, storage)
