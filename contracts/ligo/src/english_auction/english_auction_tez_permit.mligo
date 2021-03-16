#include "./english_auction_tez.mligo"

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
  | Auction of auction_without_configure_entrypoints
  | Permit_config of permit_config_param list

type permit_return = operation list * permit_storage

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

let config_storage_with_permit (p, permit_storage : permit_config_param * permit_storage)  : permit_storage = begin
  match p.optional_permit with 
    | None -> 
      let auction_storage = configure_auction_storage(p.config, Tezos.sender, permit_storage.auction_storage) in
      {permit_storage with auction_storage = auction_storage; counter = permit_storage.counter + 1n}
    | Some permit -> 
        let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (permit_storage.counter, permit.paramHash) : bytes) in
        assert_msg (Crypto.check permit.signerKey permit.signature unsigned, "INVALID_PERMIT"); 
        assert_msg (Crypto.blake2b (Bytes.pack p.config) = permit.paramHash, "Config params and param hash do not match");
        
        let configure_param = p.config in
        let storage = permit_storage.auction_storage in
        let signer_address = address_from_key permit.signerKey in
        
        let auction_storage = configure_auction_storage(p.config, signer_address, storage) in 
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
  | Permit_config pps -> configure_auction_with_permits(pps, permit_storage)