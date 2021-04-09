#include "fa2_multi_nft_token_editions.mligo"

let token_metadata (token_id, storage: nat * editions_storage) : token_metadata =
  match (Big_map.find_opt token_id storage.nft_asset_storage.assets.token_metadata) with 
    | Some md -> let edition_id = Map.find_opt ("edition_id" : string) md.token_info in 
       (match edition_id with 
        | Some edition_id -> 
            ( let edition_id = ((Bytes.unpack edition_id) : nat option) in 
            match edition_id with 
            | Some edition_id -> 
                (match (Big_map.find_opt edition_id storage.editions_metadata) with 
                | Some edition_metadata -> ({
                      token_id = token_id;
                      token_info = edition_metadata.edition_info
                   } : token_metadata)
                | None -> (failwith "FA2_TOKEN_UNDEFINED" : token_metadata))
            | None -> (failwith "FA2_TOKEN_UNDEFINED" : token_metadata))
        | None -> md )
    | None -> (failwith "FA2_TOKEN_UNDEFINED" : token_metadata)