#if !EDITIONS
#define EDITIONS

#include "../../../fa2/fa2_interface.mligo"
#include "../nft/fa2_multi_nft_manager.mligo"
#include "../nft/fa2_multi_nft_asset_simple_admin.mligo"

#if OWNER_HOOKS
"Sorry but this contract does not work with hooks yet. 
In order to add hooks, rework code so that the external operations generated by each call to the 
mint_edition_set function defined in fa2_multi_nft_manager are returned when OWNER_HOOKS is defined." 
#endif

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
      number_of_editions : nat;
      number_of_editions_to_distribute : nat;
  }

type editions_metadata = (nat, edition_metadata) big_map

type editions_storage = 
{   
    next_edition_id : nat; 
    max_editions_per_run : nat;
    editions_metadata : editions_metadata;
    nft_asset_storage : nft_asset_storage;
}

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let mint_editions ( edition_run_list , storage : mint_edition_run list * editions_storage)
  : operation list * editions_storage =
  let mint_single_edition_run : (editions_storage * mint_edition_run) -> editions_storage = 
    fun (storage, param : editions_storage * mint_edition_run) ->
      let u : unit = assert_msg(param.number_of_editions <= storage.max_editions_per_run,
         "EDITION_SET_TOO_LARGE" ) in
      let edition_metadata : edition_metadata = {
        creator = Tezos.sender;
        edition_info = param.edition_info;
        number_of_editions = param.number_of_editions;
        number_of_editions_to_distribute = param.number_of_editions
      } in
      let new_editions_metadata = Big_map.add storage.next_edition_id edition_metadata storage.editions_metadata in
      {storage with 
          next_edition_id = storage.next_edition_id + 1n;
          editions_metadata = new_editions_metadata; 
      } in
  let new_storage = List.fold mint_single_edition_run edition_run_list storage in
  ([] : operation list), new_storage

let distribute_edition_to_addresses ( edition_id, token_info, receivers, edition_metadata, storage : nat *  ((string, bytes) map) * (address list) * edition_metadata * editions_storage)
  : editions_storage = 
  let distribute_edition_to_address : ((mint_editions_param * token_id) * address) -> (mint_editions_param * token_id) = 
    fun ( (mint_editions_param, token_id), to_  : (mint_editions_param * token_id) * address) ->
      let mint_edition_param : mint_edition_param = {
          owner = to_;
          token_id = token_id;
      } in
      ((mint_edition_param :: mint_editions_param) , token_id + 1n) 
  in 
  let number_of_editions_left_after_distribution : int = edition_metadata.number_of_editions_to_distribute - (List.length receivers) in
  let u : unit = assert_msg(number_of_editions_left_after_distribution >= 0, "NO_EDITIONS_TO_DISTRIBUTE" ) in
  let intial_token_id : nat = (edition_id * storage.max_editions_per_run) + abs (edition_metadata.number_of_editions - edition_metadata.number_of_editions_to_distribute) in 
  let mint_editions_param, _ : mint_editions_param * token_id = (List.fold distribute_edition_to_address receivers (([] : mint_editions_param), intial_token_id)) in
  let new_edition_metadata : edition_metadata = {edition_metadata with number_of_editions_to_distribute = abs(number_of_editions_left_after_distribution)} in 
  let _ , nft_token_storage = mint_edition_set (mint_editions_param, storage.nft_asset_storage.assets) in
  let new_editions_metadata = Big_map.update edition_id (Some new_edition_metadata) storage.editions_metadata in
  let new_storage = {storage with nft_asset_storage = {storage.nft_asset_storage with assets = nft_token_storage};
                     editions_metadata = new_editions_metadata} in 
  new_storage 
  

let distribute_editions (distribute_list, storage : distribute_edition list * editions_storage)
  : operation list * editions_storage =
  let distribute_edition : (editions_storage * distribute_edition) -> editions_storage = 
    fun (storage, distribute_param : editions_storage * distribute_edition) -> 
        let edition_metadata : edition_metadata = (match (Big_map.find_opt distribute_param.edition_id storage.editions_metadata) with 
          | Some edition_metadata -> edition_metadata 
          | None -> (failwith "INVALID_EDITION_ID" : edition_metadata)) in 
        let u : unit = if (Tezos.sender <> edition_metadata.creator) 
            then (failwith "INVALID_DISTRIBUTOR" : unit) else () in 
        let token_info : ((string, bytes) map) =  Map.literal [(("edition_id" : string), (Bytes.pack distribute_param.edition_id))] in
        let new_editions_storage = distribute_edition_to_addresses(distribute_param.edition_id, token_info, distribute_param.receivers, edition_metadata, storage) in
        new_editions_storage
  in
  let new_storage = List.fold distribute_edition distribute_list storage in 
  ([] : operation list), new_storage

let editions_main (param, editions_storage : editions_entrypoints * editions_storage)
    : (operation  list) * editions_storage =
    match param with 
    | FA2 nft_asset_entrypoints -> 
        let ops, new_nft_asset_storage = nft_asset_main (nft_asset_entrypoints, editions_storage.nft_asset_storage) in
        ops, {editions_storage with nft_asset_storage = new_nft_asset_storage}
    | Mint_editions mint_param -> 
        let u : unit = fail_if_not_admin editions_storage.nft_asset_storage.admin in
        (mint_editions (mint_param, editions_storage))
    | Distribute_editions distribute_param -> 
        let u : unit = fail_if_paused editions_storage.nft_asset_storage.admin in
        (distribute_editions (distribute_param, editions_storage))

let sample_storage : editions_storage = {
  next_edition_id = 0n;
  editions_metadata = (Big_map.empty : editions_metadata);
  max_editions_per_run = 1000n;
  nft_asset_storage = {
    admin  = {
      admin = ("tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU" : address);
      pending_admin = (None : address option);
      paused = false;
    };
    assets = {
      ledger = (Big_map.empty : ledger);
      operators = (Big_map.empty : operator_storage);
    };
    metadata  = Big_map.literal [
      ("", 0x74657a6f732d73746f726167653a636f6e74656e74 );
      (* ("", "tezos-storage:content"); *)
      ("content", 0x00) (* bytes encoded UTF-8 JSON *)
    ];
  };
}

#endif