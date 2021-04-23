
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

#endif

  | Admin a ->
    let ops, admin = admin_main (a, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage
