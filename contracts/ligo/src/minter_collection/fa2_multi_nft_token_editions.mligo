#include "../../fa2/fa2_interface.mligo"
#include "fa2_multi_nft_token.mligo"

type mint_editions = 
[@layout:comb]
{
    token_metadata : token_metadata;
    number_of_editions : nat;
}

type editions_entrypoints = 
 | FA2 of fa2_entry_points
 | Mint_editions of mint_editions list
 | Distribute_editions of address list 

type edition_metadata =
  [@layout:comb]
  {
      initial_token_id : nat option;
      number_of_editions : nat;
      number_of_editions_to_distribute : nat;
  }

type editions_metadata = (address, edition_metadata) big_map

type editions_storage = {
    nft_storage : nft_token_storage;
    editions_metadata : editions_metadata;
}

let mint_editions ( param, storage : mint_editions list * editions_storage)
  : (operation  list) * editions_storage =
  ([] : operation list), storage

let distribute_editions ( param, storage : address list * editions_storage)
  : (operation  list) * editions_storage = 
  ([] : operation list), storage

let editions_main (param, editions_storage : editions_entrypoints * editions_storage)
    : (operation  list) * editions_storage =
    match param with 
    | FA2 fa2_entry_points -> 
        let ops, nft_storage = fa2_main (fa2_entry_points, editions_storage.nft_storage) in
        ops, {editions_storage with nft_storage = nft_storage}
    | Mint_editions mint_param -> mint_editions (mint_param, editions_storage)
    | Distribute_editions distribute_param -> distribute_editions (distribute_param, editions_storage)