#include "./english_auction_tez_api.mligo"

type permit = 
  [@layout:comb]
  {
    signerKey: key;
    signature: signature;
    paramHash: bytes;
  }

type permit_storage = 
    [@layout:comb]
  {
    auction_storage : storage;
    counter : nat;
  }

type permit_config_param =
  [@layout:comb]
  {
    config : configure_param;
    optional_permit : permit option;
  } 

type permit_auction_entrypoints = 
  | Auction of auction_entrypoints
  | Permit_config of permit_config_param list

type permit_return = operation list * permit_storage

let concat (l1, l2 : operation list * operation list) : operation list = 
  let concat = ([%Michelson ({| {UNPAIR; NIL operation; SWAP; ITER {CONS}; ITER {CONS}} |} : operation list * operation list -> operation list)]) 
  in concat (l1, l2)

let config_with_permit (p, permit_storage, selfAddress : permit_config_param * permit_storage * address)  : permit_return = begin
  match p.optional_permit with 
    | None -> 
      let ops, auction_storage = configure_auction(p.config, permit_storage.auction_storage, selfAddress) in
      ops, {permit_storage with auction_storage = auction_storage; counter = permit_storage.counter + 1n}
    | Some permit -> 
        let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (permit_storage.counter, permit.paramHash) : bytes) in
        assert_msg (Crypto.check permit.signerKey permit.signature unsigned, "INVALID_PERMIT"); 
        assert_msg (Crypto.blake2b (Bytes.pack p.config) = permit.paramHash, "Config params and param hash do not match");
        
        let configure_param = p.config in
        let storage = permit_storage.auction_storage in
        let signer_address = Tezos.address (Tezos.implicit_account (Crypto.hash_key permit.signerKey)) in
        
        assert_msg (configure_param.end_time > configure_param.start_time, "end_time must be after start_time");
        assert_msg (abs(configure_param.end_time - configure_param.start_time) <= storage.max_auction_time, "Auction time must be less than max_auction_time");
        
        assert_msg (configure_param.start_time >= Tezos.now, "Start_time must not have already passed");
        assert_msg (abs(configure_param.start_time - Tezos.now) <= storage.max_config_to_start_time, "start_time must not be greater than the sum of current time and max_config_to_start_time");
        
        assert_msg (configure_param.opening_price > 0mutez, "Opening price must be greater than 0mutez");
        assert_msg (Tezos.amount = configure_param.opening_price, "Amount must be equal to opening_price");
        assert_msg (configure_param.round_time > 0n, "Round_time must be greater than 0 seconds");
        let auction_data : auction = {
          seller = signer_address;
          current_bid = configure_param.opening_price;
          start_time = configure_param.start_time;
          round_time = int(configure_param.round_time);
          extend_time = int(configure_param.extend_time);
          asset = configure_param.asset;
          min_raise_percent = configure_param.min_raise_percent;
          min_raise = configure_param.min_raise;
          end_time = configure_param.end_time;
          highest_bidder = signer_address;
          last_bid_time = configure_param.start_time; 
        } in
        let updated_auctions : (nat, auction) big_map = Big_map.update storage.current_id (Some auction_data) storage.auctions in
        let fa2_transfers : operation list = tokens_to_operation_list(configure_param.asset, Tezos.sender, Tezos.self_address) in
        let auction_storage = {storage with auctions = updated_auctions; current_id = storage.current_id + 1n} in
        (fa2_transfers, {permit_storage with auction_storage = auction_storage; counter = permit_storage.counter + 1n})
end

let rec config_with_permits_helper (possible_permits, permit_storage, selfAddress, returnOps : (permit_config_param list * permit_storage * address * (operation list))) : permit_return =
    let permit = List.head_opt possible_permits in
    let remaining_permits = List.tail_opt possible_permits in 
    match permit with
      | Some p -> 
          let ops, new_storage = config_with_permit(p, permit_storage, selfAddress) in
          (match remaining_permits with 
            | Some rps -> 
                let concatenated_ops : operation list = concat(ops, returnOps) in
                config_with_permits_helper(rps, new_storage, selfAddress, concatenated_ops)
            | None -> (failwith "INTERNAL_ERROR" : permit_return))
      | None -> (returnOps, permit_storage)

let configure_auction_with_permits (possible_permits, permit_storage, selfAddress: (permit_config_param list * permit_storage * address)) : permit_return =
  config_with_permits_helper(possible_permits, permit_storage, selfAddress, ([] : operation list))

let english_auction_tez_permit_main (params,permit_storage : permit_auction_entrypoints * permit_storage) : permit_return = match params with
  | Auction auction -> 
      let ops, auction_storage  = english_auction_tez_api(auction, permit_storage.auction_storage, Tezos.self_address) in
      ops, {permit_storage with auction_storage = auction_storage}
  | Permit_config pps -> configure_auction_with_permits(pps, permit_storage, Tezos.self_address)