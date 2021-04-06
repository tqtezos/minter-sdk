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