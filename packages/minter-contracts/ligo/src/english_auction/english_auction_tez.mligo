#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../allowlist_common.mligo"
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
#if CONSOLATION_AUCTION
    consolation_index : nat; 
    num_losing_bidders : nat;
    consolation_tokens_sent : nat;
    consolation_token : global_token_id;
    max_consolation_winners : nat;
#endif
#if OFFCHAIN_BID
    last_bid_offchain : bool;
#endif
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
#if CONSOLATION_AUCTION
    consolation_token : global_token_id;
    max_consolation_winners : nat;
#endif
  }

type auction_without_configure_entrypoints =
  | Bid of nat
  | Cancel of nat
  | Resolve of nat
  | Admin of pauseable_admin
  | Update_allowed of allowlist_entrypoints
#if OFFCHAIN_BID
  | Offchain_bid of permit_bid_param
#endif
#if CONSOLATION_AUCTION
  | Send_consolation of (nat * nat)
#endif

type auction_entrypoints =
  | Configure of configure_param
  | AdminAndInteract of auction_without_configure_entrypoints

type storage =
  [@layout:comb]
  {
    admin : pauseable_admin_storage;
    current_id : nat;
    max_auction_time : nat;
    max_config_to_start_time : nat;
    auctions : (nat, auction) big_map;
    allowlist : allowlist;
#if FEE
    fee : fee_data;
#endif

#if CONSOLATION_AUCTION
    consolation_queue : (nat * nat, address) big_map;
    consolation_receivers : (nat * address, unit) big_map;
#endif
  }

type return = operation list * storage

#if CONSOLATION_AUCTION

let add_consolation_winner (auction_id, consolation_index, bidder, storage : nat * nat * address * storage) 
  : storage * bool =
  let new_bidder_added : bool = 
    not (Big_map.mem (auction_id, bidder) storage.consolation_receivers) in 
  let new_receiver_bm : (nat * address, unit) big_map = 
    Big_map.update (auction_id, bidder) (Some unit) storage.consolation_receivers in 
  let new_queue_bm : (nat * nat, address) big_map =
    Big_map.update (auction_id, consolation_index) (Some bidder) storage.consolation_queue in 
  let new_storage : storage = 
    { storage with 
      consolation_queue = new_queue_bm;
      consolation_receivers = new_receiver_bm;
    } in 
  (new_storage, new_bidder_added)

let remove_consolation_winner (auction_id, consolation_index, bidder, storage : nat * nat * address * storage) 
  : storage =
  let new_receiver_bm : (nat * address, unit) big_map = 
    Big_map.update (auction_id, bidder) (None : unit option) storage.consolation_receivers in 
  let new_queue_bm : (nat * nat, address) big_map =
    Big_map.update (auction_id, consolation_index) (None : address option) storage.consolation_queue in 
  ({ storage with 
    consolation_queue = new_queue_bm;
    consolation_receivers = new_receiver_bm;
   } : storage)

let all_consolation_tokens_sent (auction : auction) : bool = 
  auction.consolation_tokens_sent = auction.num_losing_bidders ||
  auction.consolation_tokens_sent = auction.max_consolation_winners

#endif

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
  auction.highest_bidder = auction.seller

let dont_return_bid (auction : auction) : bool =
  first_bid(auction)

#if OFFCHAIN_BID
  || auction.last_bid_offchain 
#endif

let valid_bid_amount (auction, bid_amount : auction * tez) : bool =
  (bid_amount >= (auction.current_bid + (percent_of_bid_tez (auction.min_raise_percent, auction.current_bid)))) ||
  (bid_amount >= auction.current_bid + auction.min_raise)                                            ||
  ((bid_amount >= auction.current_bid) && first_bid(auction))


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

    assert_msg (configure_param.opening_price > 0mutez, "INVALID_OPENING_PRICE");
    tez_stuck_guard("CONFIGURE");
    assert_msg (configure_param.round_time > 0n, "INVALID_ROUND_TIME");
    assert_msg(configure_param.min_raise_percent > 0n && configure_param.min_raise > 0mutez, "INVALID_RAISE_CONFIGURATION");

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
#if OFFCHAIN_BID
      last_bid_offchain = false;
#endif
#if CONSOLATION_AUCTION
      consolation_index = 1n;
      consolation_token = configure_param.consolation_token;
      consolation_tokens_sent = 0n;
      num_losing_bidders = 0n;
      max_consolation_winners = configure_param.max_consolation_winners;
#endif
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
#if CONSOLATION_AUCTION
  let consolation_transfer : operation = 
    transfer_tokens_in_single_contract Tezos.sender Tezos.self_address 
                                      ({fa2_address = configure_param.consolation_token.fa2_address;
                                        fa2_batch = [{token_id = configure_param.consolation_token.token_id;
                                                      amount = configure_param.max_consolation_winners;}]
                                       }) in 
  let fa2_transfers = consolation_transfer :: fa2_transfers in 
#endif 
  (fa2_transfers, new_storage)


let resolve_auction(auction_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(auction_id, storage) in
    assert_msg (auction_ended(auction) , "AUCTION_NOT_ENDED");
    tez_stuck_guard("RESOLVE");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.highest_bidder) in

#if !FEE

    let oplist = if dont_return_bid(auction) then
      fa2_transfers else
      let send_final_bid : operation = transfer_tez(auction.current_bid, auction.seller) in
      (send_final_bid :: fa2_transfers) in

#else
    let oplist = 
      if dont_return_bid(auction)
      then fa2_transfers 
      else let fee : tez = percent_of_price_tez (storage.fee.fee_percent, auction.current_bid) in //ceiling not used
      let op_list =
        (if fee <> 0mutez
         then
           let tx_fee : operation = transfer_tez(fee, storage.fee.fee_address) in
           tx_fee :: fa2_transfers
         else fa2_transfers) in
      let op_list =
        (let sale_price_minus_fee = auction.current_bid - fee in 
         if sale_price_minus_fee <> 0mutez
         then
           let tx_price = transfer_tez(sale_price_minus_fee, auction.seller) in
           tx_price :: op_list
         else op_list) in
      op_list 
    in 
#endif

#if CONSOLATION_AUCTION
    assert_msg (all_consolation_tokens_sent(auction), "CONSOLATION_NOT_SENT");
    let oplist = 
      if auction.consolation_tokens_sent >= auction.max_consolation_winners 
      then oplist 
      else 
          let remaining_consolation_tokens : nat = 
            abs (auction.max_consolation_winners - auction.consolation_tokens_sent) in 
          let send_consolation_tokens : operation = 
            transfer_tokens_in_single_contract Tezos.self_address auction.seller
                  ({fa2_address = auction.consolation_token.fa2_address;
                    fa2_batch = [{token_id = auction.consolation_token.token_id;
                                  amount = remaining_consolation_tokens;}]
                   }) in
          send_consolation_tokens :: oplist    
    in       
#endif

    let updated_auctions = Big_map.remove auction_id storage.auctions in
    (oplist, {storage with auctions = updated_auctions})
  end

let cancel_auction(auction_id, storage : nat * storage) : return = begin
    (fail_if_paused storage.admin);
    let auction : auction = get_auction_data(auction_id, storage) in
    let is_seller : bool = Tezos.sender = auction.seller in
    let v : unit = if is_seller then ()
          else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
    assert_msg (not auction_ended(auction), "AUCTION_ENDED");
    tez_stuck_guard("CANCEL");

    let fa2_transfers : operation list = transfer_tokens(auction.asset, Tezos.self_address, auction.seller) in
    let op_list : operation list = if dont_return_bid(auction) then
      fa2_transfers else
      let return_bid : operation = transfer_tez(auction.current_bid, auction.highest_bidder) in 
      (return_bid :: fa2_transfers) in
#if CONSOLATION_AUCTION
    let op_list = 
        let return_consolation_tokens : operation = 
          transfer_tokens_in_single_contract Tezos.self_address auction.seller
                ({fa2_address = auction.consolation_token.fa2_address;
                  fa2_batch = [{token_id = auction.consolation_token.token_id;
                                amount = auction.max_consolation_winners;}]
                 }) in
        return_consolation_tokens :: op_list    
    in       
#endif
    let updated_auctions = Big_map.remove auction_id storage.auctions in
    (op_list, {storage with auctions = updated_auctions})
  end

let place_bid(  auction_id 
              , auction 
              , bid_amount 
              , bidder 
              , storage
#if OFFCHAIN_BID
              , is_offchain
#endif
              : nat 
              * auction 
              * tez 
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
    assert_msg(bidder <> auction.highest_bidder, "NO_SELF_OUTBIDS");
    (if not valid_bid_amount(auction, bid_amount)
      then ([%Michelson ({| { FAILWITH } |} : string * (tez * tez * address * timestamp * timestamp) -> unit)] ("INVALID_BID_AMOUNT", (auction.current_bid, Tezos.amount, auction.highest_bidder, auction.last_bid_time, Tezos.now)) : unit)
      else ());
    let op_list : operation list = if dont_return_bid(auction) then
      ([] : operation list) else
      let return_bid : operation = transfer_tez(auction.current_bid,  auction.highest_bidder) in
      [return_bid] in
#if CONSOLATION_AUCTION
    let (auction, storage) : auction * storage = 
        if first_bid(auction)
        then (auction, storage)
        else let (new_storage, new_bidder_added) : storage * bool = 
               add_consolation_winner(auction_id, auction.consolation_index, auction.highest_bidder, storage) in 
             let num_losing_bidders = 
               if new_bidder_added 
               then auction.num_losing_bidders + 1n
               else auction.num_losing_bidders
               in
             let new_auction : auction = 
               {auction with 
                 consolation_index = auction.consolation_index + 1n;
                 num_losing_bidders = num_losing_bidders;
               } in 
             (new_auction, new_storage)  
    in 
#endif
    let new_end_time = if auction.end_time - Tezos.now <= auction.extend_time then
      Tezos.now + auction.extend_time else auction.end_time in
    let updated_auction_data = {auction with current_bid = bid_amount; highest_bidder = bidder; 
                                last_bid_time = Tezos.now; end_time = new_end_time;
#if OFFCHAIN_BID
                                last_bid_offchain = is_offchain; 
#endif
                               } in
    let updated_auctions = Big_map.update auction_id (Some updated_auction_data) storage.auctions in
    (op_list , {storage with auctions = updated_auctions})
  end

let place_bid_onchain(auction_id, storage : nat * storage) : return = begin
    let bid_amount = Tezos.amount in 
    let bidder = Tezos.sender in 
    let auction : auction = get_auction_data(auction_id, storage) in 
    let bid_placed_offchain : bool = false in 
    let (ops, new_storage) = place_bid(auction_id, auction, bid_amount, bidder, storage
#if OFFCHAIN_BID
      , bid_placed_offchain
#endif
      ) in 
    (ops, new_storage)
  end

#if OFFCHAIN_BID
let place_bid_offchain(offchain_bid_data, bidder, storage : offchain_bid_data * address * storage) : return = begin
    let { auction_id = auction_id;
          bid_amount = bid_amount;
        } = offchain_bid_data in 
    let auction : auction = get_auction_data(auction_id, storage) in
    let bid_placed_offchain : bool = true in 
    let (ops, new_storage) = place_bid(auction_id, auction, bid_amount, bidder, storage, bid_placed_offchain ) in 
    (ops, new_storage)
  end

let bid_with_permit (p, storage : permit_bid_param * storage)  : return = begin 
    fail_if_not_admin(storage.admin);
    let {offchain_bid_data = offchain_bid_data;
         permit = permit; } = p in 
    let param_hash = Crypto.blake2b (Bytes.pack offchain_bid_data) in 
    let v : unit = check_permit (permit, 0n, param_hash) in  (*Always set counter to 0*)
    let bidder = address_from_key (p.permit.signerKey) in
    let (ops, storage) = place_bid_offchain(offchain_bid_data, bidder, storage) in 
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

#if CONSOLATION_AUCTION

let rec send_highest_consolation_tokens(txs, auction_id, auction, consolation_index, send_qty, tokens_sent, storage 
   : transfer_destination list * nat * auction * nat * nat * nat * storage) 
   : (transfer_destination list * nat * storage) = begin 
    
    let bidder_address : address = 
      match (Map.find_opt (auction_id, consolation_index) storage.consolation_queue) with 
        | Some add -> add
        | None -> (failwith "INVALID_CONSOLATION_ID" : address)
      in 

    let is_duplicate_token : bool = not Big_map.mem (auction_id, bidder_address) storage.consolation_receivers in 

    let token_id = auction.consolation_token.token_id in 

#if POSITIONAL_AUCTION
    let token_id = token_id + tokens_sent in 
#endif

    let tx : transfer_destination = {
      to_ = bidder_address;
      token_id = token_id;
      amount = 1n;
    } in 

    let updated_storage = remove_consolation_winner(auction_id, consolation_index, bidder_address, storage) in 
    
    let (updated_txs, tokens_sent) : transfer_destination list * nat = 
      if is_duplicate_token 
      then (txs, tokens_sent)
      else (tx :: txs, tokens_sent + 1n)
      in 
    
    let consolation_index : int = int(consolation_index) - 1 in 

    if tokens_sent >= send_qty || consolation_index <= 0 || tokens_sent >= auction.max_consolation_winners
    then (updated_txs, tokens_sent, storage)
    else send_highest_consolation_tokens(updated_txs, auction_id, auction, abs(consolation_index), send_qty, tokens_sent, updated_storage)
  end

let send_consolation(auction_id, distribute_qty, storage : nat * nat * storage) : return = begin
  fail_if_not_admin(storage.admin);
  let auction : auction = get_auction_data(auction_id, storage) in
  let consolation_tokens_sent : nat = auction.consolation_tokens_sent in  
  let conoslation_tokens_remaining : bool = not all_consolation_tokens_sent(auction) in 
  assert_msg(auction_ended(auction), "AUCTION_NOT_ENDED");
  assert_msg(distribute_qty > 0n, "DISTIBUTE_QTY_MUST_BE_NONZERO");
  assert_msg(conoslation_tokens_remaining, "MAX_CONSOLATION_TOKENS_SENT");
  let highest_consolation_index : nat = abs(auction.consolation_index - consolation_tokens_sent - 1n) in 
  let (transfers, updated_consolation_tokens_sent, new_storage) : transfer_destination list * nat * storage = 
    send_highest_consolation_tokens(([] : transfer_destination list), auction_id, auction, highest_consolation_index, distribute_qty, consolation_tokens_sent, storage)
    in 
  let transfer_param = [{from_ = Tezos.self_address; txs = transfers}] in
  let c = address_to_contract_transfer_entrypoint(auction.consolation_token.fa2_address) in
  let op : operation = (Tezos.transaction transfer_param 0mutez c) in 
  let updated_auction_data : auction = 
    {auction with 
       consolation_tokens_sent = updated_consolation_tokens_sent;
    } in 
  let updated_auctions : (nat, auction) big_map = 
    Big_map.update auction_id (Some updated_auction_data) new_storage.auctions in
  ([op], {new_storage with auctions = updated_auctions})
  end

#endif

let english_auction_tez_no_configure (p,storage : auction_without_configure_entrypoints * storage) : return =
  match p with
    | Bid auction_id -> place_bid_onchain(auction_id, storage)
    | Cancel auction_id -> cancel_auction(auction_id, storage)
    | Resolve auction_id -> resolve_auction(auction_id, storage)
    | Admin a -> admin(a, storage)
    | Update_allowed a -> update_allowed(a, storage)
#if OFFCHAIN_BID
    | Offchain_bid permit -> bid_with_permit(permit, storage)
#endif
#if CONSOLATION_AUCTION
    | Send_consolation consolation_param -> 
        let (auction_id, distribute_qty) = consolation_param in 
        send_consolation(auction_id, distribute_qty, storage)
#endif 

let english_auction_tez_main (p,storage : auction_entrypoints * storage) : return = match p with
    | Configure config -> configure_auction(config, storage)
    | AdminAndInteract ai -> english_auction_tez_no_configure(ai, storage)
