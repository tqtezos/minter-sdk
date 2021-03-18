#include "../../fa2/fa2_interface.mligo"
#include "fa2_multi_nft_asset.mligo"

type mint_editions = 
[@layout:comb]
{
    token_metadata : token_metadata;
    number_of_editions : nat;
}

type editions_entrypoints = 
 | FA2 of nft_asset_entrypoints
 | Mint_editions of mint_editions list
 | Distribute_editions of (nat * address list) list

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

type editions_storage = {
    nft_asset_storage : nft_asset_storage;
    editions_metadata : editions_metadata;
    current_edition_id : nat; 
}

let rec mint_editions_storage_update ( editions_list , storage : mint_editions list * editions_storage)
  : editions_storage =
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
        editions_metadata = new_editions_metadata; 
        nft_asset_storage = {storage.nft_asset_storage with assets = new_asset_storage}
    } in
  (match List.head_opt editions_list with 
    | Some editions -> let new_storage = mint_single_edition_set(editions, storage) in 
        (match List.tail_opt editions_list with
          | Some remaining_editions -> mint_editions_storage_update (remaining_editions, new_storage)
          | None -> (failwith "INTERNAL_ERROR" : editions_storage))
    | None -> storage)
(*
let rec distribute_editions_storage_update (to_list, storage : (address list) * editions_storage) 
  : editions_storage = 
  let distribute_single_edition : (address * editions_storage) -> editions_storage = fun (to_, storage : address * editions_storage) ->
    let new_token_id : nat = match 
  match List.head_opt to_list with 
    | Some to_ -> let 
    | None -> storage *)

let mint_editions ( param, storage : mint_editions list * editions_storage)
  : (operation  list) * editions_storage =
  let new_storage = mint_editions_storage_update (param, storage) in 
  ([] : operation list), new_storage 
  
let distribute_editions ( param, storage : (nat * address list) list * editions_storage)
  : (operation  list) * editions_storage = 
  ([] : operation list), storage

let editions_main (param, editions_storage : editions_entrypoints * editions_storage)
    : (operation  list) * editions_storage =
    match param with 
    | FA2 nft_asset_entrypoints -> 
        let ops, new_nft_asset_storage = nft_asset_main (nft_asset_entrypoints, editions_storage.nft_asset_storage) in
        ops, {editions_storage with nft_asset_storage = new_nft_asset_storage}
    | Mint_editions mint_param -> mint_editions (mint_param, editions_storage)
    | Distribute_editions distribute_param -> distribute_editions (distribute_param, editions_storage)