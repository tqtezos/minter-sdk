#include "fa2_multi_nft_token_editions.mligo"

let token_metadata (token_id, storage: nat * editions_storage) : token_metadata =
  match (Big_map.find_opt token_id storage.nft_asset_storage.assets.ledger) with 
    | Some addr -> 
            let edition_id = token_id_to_edition_id(token_id, storage) in 
            (match (Big_map.find_opt edition_id storage.editions_metadata) with 
            | Some edition_metadata -> ({
                  token_id = token_id;
                  token_info = edition_metadata.edition_info
               } : token_metadata)
            | None -> (failwith "FA2_TOKEN_UNDEFINED" : token_metadata))
    | None -> (failwith "FA2_TOKEN_UNDEFINED" : token_metadata)