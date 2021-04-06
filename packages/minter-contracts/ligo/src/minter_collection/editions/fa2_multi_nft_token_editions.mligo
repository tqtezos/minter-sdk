#include "../../../fa2/fa2_interface.mligo"
#include "../nft/fa2_multi_nft_manager.mligo"
#include "../nft/fa2_multi_nft_asset_simple_admin.mligo"

type mint_edition_run = 
[@layout:comb]
{
    edition_info : ((string, bytes) map);
    number_of_editions : nat;
}

type distribute_edition = 
[@layout:comb] 
{
    edition_id : nat; 
    receivers : address list;
}

type editions_entrypoints = 
 | FA2 of nft_asset_entrypoints
 | Mint_editions of mint_edition_run list
 | Distribute_editions of distribute_edition list

type edition_metadata =
[@layout:comb]
  {   
      creator : address;
      edition_info: ((string, bytes) map);
      initial_token_id : nat;
      number_of_editions : nat;
      number_of_editions_to_distribute : int;
  }

type editions_metadata = (nat, edition_metadata) big_map

type editions_storage = 
{   
    current_edition_id : nat; 
    editions_metadata : editions_metadata;
    nft_asset_storage : nft_asset_storage;
}

let rec mint_editions ( editions_list , storage : mint_edition_run list * editions_storage)
  : operation list * editions_storage =
  let mint_single_edition_run : (mint_edition_run * editions_storage) -> editions_storage = fun (param, storage : mint_edition_run * editions_storage) ->  
    let edition_metadata : edition_metadata = {
      creator = Tezos.sender;
      edition_info = param.edition_info;
      initial_token_id = storage.nft_asset_storage.assets.next_token_id;
      number_of_editions = param.number_of_editions;
      number_of_editions_to_distribute = int(param.number_of_editions) 
    } in
    let new_editions_metadata = Big_map.add storage.current_edition_id edition_metadata storage.editions_metadata in
    let new_asset_storage = {storage.nft_asset_storage.assets with next_token_id = storage.nft_asset_storage.assets.next_token_id + param.number_of_editions} in 
    {storage with 
        current_edition_id = storage.current_edition_id + 1n;
        editions_metadata = new_editions_metadata; 
        nft_asset_storage = {storage.nft_asset_storage with assets = new_asset_storage}
    } in
  (match editions_list with 
    | edition_run :: remaining_editions -> 
        let new_storage = mint_single_edition_run(edition_run, storage) in 
        mint_editions (remaining_editions, new_storage)
    | [] -> ([] : operation list), storage)

let distribute_edition_to_address (edition_id, edition_metadata, to_, token_id, storage : nat * edition_metadata * address * nat * editions_storage) 
  : editions_storage * edition_metadata = 
  let new_edition_metadata = {edition_metadata with number_of_editions_to_distribute = edition_metadata.number_of_editions_to_distribute - 1} in
  let mint_token_param : mint_token_param = {
      owner = to_;
      token_metadata = {
        token_id = token_id;
        token_info = Map.literal [(("edition_id" : string), (Bytes.pack edition_id))];
      }
  } in
  let _ , nft_token_storage = mint_edition (mint_token_param, storage.nft_asset_storage.assets) in
  {storage with nft_asset_storage = {storage.nft_asset_storage with assets = nft_token_storage}}, new_edition_metadata

let rec distribute_edition_to_addresses ( edition_id, receivers, edition_metadata, token_id, storage : nat * (address list) * edition_metadata * nat * editions_storage)
  : editions_storage * edition_metadata = 
  match receivers with 
    | to_ :: remaining_receivers -> 
       let new_storage, new_edition_metadata = distribute_edition_to_address (edition_id, edition_metadata, to_, token_id, storage) in 
       distribute_edition_to_addresses (edition_id, remaining_receivers, new_edition_metadata, token_id + 1n, new_storage)
    | [] -> 
        if (edition_metadata.number_of_editions_to_distribute < 0) then 
             (failwith "NO_EDITIONS_TO_DISTRIBUTE" : editions_storage * edition_metadata) else 
             storage, edition_metadata 

let rec distribute_editions (distribute_list, storage : distribute_edition list * editions_storage)
  : operation list * editions_storage =
  match distribute_list with 
    | distribute_param :: remaining_distributions ->
        let edition_metadata : edition_metadata = (match (Big_map.find_opt distribute_param.edition_id storage.editions_metadata) with 
          | Some edition_metadata -> edition_metadata 
          | None -> (failwith "INVALID_EDITION_ID" : edition_metadata)) in 
        let token_id = edition_metadata.initial_token_id + abs (edition_metadata.number_of_editions - edition_metadata.number_of_editions_to_distribute) in 
        let new_editions_storage, new_edition_metadata = distribute_edition_to_addresses(distribute_param.edition_id, distribute_param.receivers, edition_metadata, token_id, storage) in
        let new_editions_metadata = Big_map.update distribute_param.edition_id (Some new_edition_metadata) new_editions_storage.editions_metadata in
        let new_storage = {new_editions_storage with editions_metadata = new_editions_metadata} in 
        (distribute_editions (remaining_distributions, new_storage))
    | [] -> ([] : operation list), storage 

let editions_main (param, editions_storage : editions_entrypoints * editions_storage)
    : (operation  list) * editions_storage =
    match param with 
    | FA2 nft_asset_entrypoints -> 
        let ops, new_nft_asset_storage = nft_asset_main (nft_asset_entrypoints, editions_storage.nft_asset_storage) in
        ops, {editions_storage with nft_asset_storage = new_nft_asset_storage}
    | Mint_editions mint_param -> 
        let u : unit = fail_if_not_admin editions_storage.nft_asset_storage.admin in
        let u = fail_if_paused editions_storage.nft_asset_storage.admin in
        (mint_editions (mint_param, editions_storage))
    | Distribute_editions distribute_param -> 
        let u : unit = fail_if_paused editions_storage.nft_asset_storage.admin in
        (distribute_editions (distribute_param, editions_storage))

let sample_storage : editions_storage = {
  current_edition_id = 0n;
  editions_metadata = (Big_map.empty : editions_metadata);
  nft_asset_storage = {
    admin  = {
      admin = ("tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU" : address);
      pending_admin = (None : address option);
      paused = false;
    };
    assets = {
      ledger = (Big_map.empty : ledger);
      token_metadata = (Big_map.empty : nft_meta);
      next_token_id = 0n;
      operators = (Big_map.empty : operator_storage);
    };
    metadata  = Big_map.literal [
      ("", Bytes.pack "tezos-storage:content" );
      (* ("", 0x74657a6f732d73746f726167653a636f6e74656e74); *)
      ("content", 0x00) (* bytes encoded UTF-8 JSON *)
    ];
  };
}