#include "../../../fa2/fa2_interface.mligo"
#include "../fa2_multi_nft_manager.mligo"
#include "../fa2_multi_nft_asset.mligo"
#include "../../../fa2_modules/pauseable_admin_option.mligo"

type mint_editions = 
[@layout:comb]
{
    token_metadata : token_metadata;
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
 | Mint_editions of mint_editions list
 | Distribute_editions of distribute_edition list

type edition_metadata =
[@layout:comb]
  {   
      creator : address;
      token_metadata : token_metadata;
      initial_token_id : nat;
      number_of_editions : nat;
      number_of_editions_to_distribute : nat;
  }

type editions_metadata = (nat, edition_metadata) big_map

type editions_storage = 
{   
    current_edition_id : nat; 
    editions_metadata : editions_metadata;
    nft_asset_storage : nft_asset_storage;
}

let rec mint_editions ( editions_list , storage : mint_editions list * editions_storage)
  : operation list * editions_storage =
  let mint_single_edition_set : (mint_editions * editions_storage) -> editions_storage = fun (param, storage : mint_editions * editions_storage) ->  
    let edition_metadata : edition_metadata = {
      creator = Tezos.sender;
      token_metadata = param.token_metadata;
      initial_token_id = storage.nft_asset_storage.assets.next_token_id;
      number_of_editions = param.number_of_editions;
      number_of_editions_to_distribute = param.number_of_editions 
    } in
    let new_editions_metadata = Big_map.add storage.current_edition_id edition_metadata storage.editions_metadata in
    let new_asset_storage = {storage.nft_asset_storage.assets with next_token_id = storage.nft_asset_storage.assets.next_token_id + param.number_of_editions} in 
    {storage with 
        current_edition_id = storage.current_edition_id + 1n;
        editions_metadata = new_editions_metadata; 
        nft_asset_storage = {storage.nft_asset_storage with assets = new_asset_storage}
    } in
  (match List.head_opt editions_list with 
    | Some editions -> let new_storage = mint_single_edition_set(editions, storage) in 
        (match List.tail_opt editions_list with
          | Some remaining_editions -> mint_editions (remaining_editions, new_storage)
          | None -> (failwith "INTERNAL_ERROR" : operation list * editions_storage))
    | None -> ([] : operation list), storage)

let distribute_edition_to_address (edition_metadata, to_, storage : edition_metadata * address * editions_storage) 
  : editions_storage * edition_metadata = 
  let token_id = if (edition_metadata.number_of_editions_to_distribute <= 0n) then 
       (failwith "NO_EDITIONS_TO_DISTRIBUTE" : nat) else 
       (edition_metadata.initial_token_id + abs (edition_metadata.number_of_editions - edition_metadata.number_of_editions_to_distribute)) in 
  let new_edition_metadata = {edition_metadata with number_of_editions_to_distribute = abs(edition_metadata.number_of_editions_to_distribute - 1)} in
  let mint_token_param : mint_token_param = {
      token_metadata = {
          token_id = token_id;
          token_info = (Map.empty : (string, bytes) map); 
      };
      owner = to_;
  } in
  let mint_tokens_param : mint_tokens_param = [mint_token_param] in 
  let _ , nft_token_storage = mint_tokens (mint_tokens_param, storage.nft_asset_storage.assets) in
  {storage with nft_asset_storage = {storage.nft_asset_storage with assets = nft_token_storage}}, new_edition_metadata

let rec distribute_edition_to_addresses ( receivers, edition_metadata, storage : (address list) * edition_metadata * editions_storage)
  : editions_storage * edition_metadata = 
  match (List.head_opt receivers) with 
    | Some to_ -> let new_storage, new_edition_metadata = distribute_edition_to_address (edition_metadata, to_, storage) in 
        (match (List.tail_opt receivers) with 
            | Some remaining_receivers -> distribute_edition_to_addresses (remaining_receivers, new_edition_metadata, new_storage)
            | None -> (failwith "INTERNAL_ERROR" : editions_storage * edition_metadata))   
    | None -> storage, edition_metadata 

let rec distribute_editions (distribute_list, storage : distribute_edition list * editions_storage)
  : operation list * editions_storage =
  match (List.head_opt distribute_list) with 
    | Some distribute_param ->
        let edition_metadata : edition_metadata = (match (Big_map.find_opt distribute_param.edition_id storage.editions_metadata) with 
          | Some edition_metadata -> edition_metadata 
          | None -> (failwith "INVALID_EDITION_ID" : edition_metadata)) in 
        let new_editions_storage, new_edition_metadata = distribute_edition_to_addresses(distribute_param.receivers, edition_metadata, storage) in
        let new_editions_metadata = Big_map.update distribute_param.edition_id (Some new_edition_metadata) new_editions_storage.editions_metadata in
        let new_storage = {new_editions_storage with editions_metadata = new_editions_metadata} in 
        let remaining_receivers : distribute_edition list = (match (List.tail_opt distribute_list) with 
          | Some remaining_editions -> remaining_editions
          | None -> (failwith "INTERNAL_ERROR" : distribute_edition list)) in 
        (distribute_editions (remaining_receivers, new_storage))
    | None -> ([] : operation list), storage 

let editions_main (param, editions_storage : editions_entrypoints * editions_storage)
    : (operation  list) * editions_storage =
    match param with 
    | FA2 nft_asset_entrypoints -> 
        let ops, new_nft_asset_storage = nft_asset_main (nft_asset_entrypoints, editions_storage.nft_asset_storage) in
        ops, {editions_storage with nft_asset_storage = new_nft_asset_storage}
    | Mint_editions mint_param -> 
        (*TODO: Move to new admin after Euguene's merge*)
        let u : unit = fail_if_not_admin editions_storage.nft_asset_storage.admin (None : string option) in
        let u = fail_if_paused editions_storage.nft_asset_storage.admin in
        (mint_editions (mint_param, editions_storage))
    | Distribute_editions distribute_param -> 
        let u : unit = fail_if_paused editions_storage.nft_asset_storage.admin in
        (distribute_editions (distribute_param, editions_storage))