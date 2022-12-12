
#include "fa2_multi_nft_token.mligo"
#include "fa2_multi_nft_manager.mligo"

type nft_asset_storage = {
  assets : nft_token_storage;
  admin : admin_storage;
  metadata: (string, bytes) big_map; (* contract metadata *)
}

#if !EDITIONS

type nft_asset_entrypoints =
  | Assets of fa2_entry_points
  | Mint of mint_tokens_param
  | Burn of (token_id * bytes)
  | Update_metadata of (token_metadata list)
  | Admin of admin_entrypoints

#else

type nft_asset_entrypoints =
  | Assets of fa2_entry_points
  | Admin of admin_entrypoints

#endif


let nft_asset_main (param, storage : nft_asset_entrypoints * nft_asset_storage)
    : operation list * nft_asset_storage =
  match param with
  | Assets fa2 ->
    let u = fail_if_paused(storage.admin) in
    let ops, new_assets = fa2_main (fa2, storage.assets) in
    let new_storage = { storage with assets = new_assets; } in
    ops, new_storage

#if !EDITIONS

  | Mint mp ->
    let u = fail_if_not_admin storage.admin in
    let ops, new_assets = mint_tokens (mp, storage.assets) in
    let new_storage = { storage with assets = new_assets;} in
    ops, new_storage


  (** Check 'symbol' is the given symbol and remove token from ledger and
      token_metadata (operator only) *)
  | Burn token_to_burn_and_symbol ->
    // let u = fail_if_not_admin storage.admin in
    let token_to_burn, token_to_burn_symbol : token_id * bytes = token_to_burn_and_symbol in

    // delete token from token_metadata and return its token_metadata for assertions
    let token_to_burn_metadata_opt, new_token_metadata : token_metadata option * nft_meta =
      Big_map.get_and_update token_to_burn (None : token_metadata option) storage.assets.token_metadata in

    // assert token_metadata exists and its "symbol" field is token_to_burn_symbol
    let burn_token : address option = match token_to_burn_metadata_opt with
    | None -> (failwith "WRONG_ID" : address option)
    | Some token_to_burn_metadata ->
      if Map.find_opt "symbol" token_to_burn_metadata.token_info = Some token_to_burn_symbol
        then (None : address option)
        else (failwith "WRONG_SYMBOL" : address option)
    // delete token from ledger
    in let token_to_burn_owner_opt, new_ledger : address * ledger =
      Big_map.get_and_update token_to_burn burn_token storage.assets.ledger in

    // ensure sender is an operator for the owner of the token
    let operations : operation list = match token_to_burn_owner_opt with
    | None -> (failwith "WRONG_ID" : operation list)
    | Some token_to_burn_owner ->

      if Big_map.mem (token_to_burn_owner, (Tezos.sender, token_to_burn)) storage.assets.operators
      then ([] : operation list)
      else (failwith "NOT_OPERATOR" : operation list)

    in let new_assets : nft_token_storage = { storage.assets with
                                              ledger = new_ledger;
                                              token_metadata = new_token_metadata } in
    operations, { storage with assets = new_assets}


  | Update_metadata token_metadatas ->
    let u = fail_if_not_admin storage.admin in
    let new_nft_meta : nft_meta = List.fold_left
      (fun (nft_meta_acc, metadata : nft_meta * token_metadata) ->
        Big_map.update metadata.token_id (Some metadata) nft_meta_acc)
      storage.assets.token_metadata
      token_metadatas in
    let new_storage = { storage with assets = { storage.assets with token_metadata = new_nft_meta } } in
    ([] : operation list), new_storage

#endif

  | Admin a ->
    let ops, admin = admin_main (a, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage

