#include "./english_auction_tez.mligo"

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
  | Auction of auction_without_configure_entrypoints
  | Permit_configure of permit_config_param list

type permit_return = operation list * permit_storage
 
let config_storage_with_permit (p, permit_storage : permit_config_param * permit_storage)  : permit_storage = begin
  let seller = match p.optional_permit with 
    | None -> Tezos.sender
    | Some permit ->
        let param_hash = Crypto.blake2b (Bytes.pack p.config) in 
        let u : unit = check_permit (permit, permit_storage.counter, param_hash) in
        address_from_key permit.signerKey in
  let auction_storage = configure_auction_storage(p.config, seller, permit_storage.auction_storage) in
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
      let ops, auction_storage  = english_auction_tez_no_configure(auction, permit_storage.auction_storage) in
      ops, {permit_storage with auction_storage = auction_storage}
  | Permit_configure pps -> configure_auction_with_permits(pps, permit_storage)