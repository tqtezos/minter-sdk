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

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

let config_storage_with_permit (p, permit_storage : permit_config_param * permit_storage)  : permit_storage = begin
  match p.optional_permit with 
    | None -> 
      let auction_storage = configure_auction_storage(p.config, permit_storage.auction_storage, Tezos.self_address) in
      {permit_storage with auction_storage = auction_storage; counter = permit_storage.counter + 1n}
    | Some permit -> 
        let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (permit_storage.counter, permit.paramHash) : bytes) in
        assert_msg (Crypto.check permit.signerKey permit.signature unsigned, "INVALID_PERMIT"); 
        assert_msg (Crypto.blake2b (Bytes.pack p.config) = permit.paramHash, "Config params and param hash do not match");
        
        let configure_param = p.config in
        let storage = permit_storage.auction_storage in
        let signer_address = address_from_key permit.signerKey in
        
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
        let auction_storage = {storage with auctions = updated_auctions; current_id = storage.current_id + 1n} in
        {permit_storage with auction_storage = auction_storage; counter = permit_storage.counter + 1n}
end

let rec config_with_permits_helper (params, permit_storage, op_list : (permit_config_param list * permit_storage * operation list)) : permit_return =
    let param = List.head_opt params in
    let remaining_params = List.tail_opt params in 
    match param with
      | Some p -> 
          let new_storage = config_storage_with_permit(p, permit_storage) in
          let transfer_from = (match p.optional_permit with 
            | Some permit -> address_from_key permit.signerKey
            | None -> Tezos.sender) in
          let ops = tokens_list_to_operation_list_append(transfer_from, Tezos.self_address, p.config.asset, op_list) in
          (match remaining_params with 
            | Some rps -> 
                config_with_permits_helper(rps, new_storage, ops)
            | None -> (failwith "INTERNAL_ERROR" : permit_return))
      | None -> (op_list, permit_storage)

let configure_auction_with_permits (possible_permits, permit_storage: (permit_config_param list * permit_storage)) : permit_return =
  let ops, storage = config_with_permits_helper(possible_permits, permit_storage, ([] : operation list)) in
  (ops, storage)

let english_auction_tez_permit_main (params,permit_storage : permit_auction_entrypoints * permit_storage) : permit_return = match params with
  | Auction auction -> 
      let ops, auction_storage  = english_auction_tez_api(auction, permit_storage.auction_storage, Tezos.self_address) in
      ops, {permit_storage with auction_storage = auction_storage}
  | Permit_config pps -> configure_auction_with_permits(pps, permit_storage)