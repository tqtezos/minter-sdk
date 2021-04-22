#if !FA2_MULTI_NFT_MINTER

#define FA2_MULTI_NFT_MINTER

#include "fa2_multi_nft_token.mligo"

type minted1 = {
  storage : nft_token_storage;
  reversed_txs : transfer_destination_descriptor list;
}

#if !EDITIONS 

type mint_token_param =
[@layout:comb]
{
  token_metadata: token_metadata;
  owner : address;
}

type mint_tokens_param = mint_token_param list

let update_meta_and_create_txs (param, storage
    : mint_tokens_param * nft_token_storage ) : minted1 =
  let seed1 : minted1 = {
    storage = storage;
    reversed_txs = ([] : transfer_destination_descriptor list);
  } in
  List.fold
    (fun (acc, t : minted1 * mint_token_param) ->
      let new_token_id = t.token_metadata.token_id in
      if (Big_map.mem new_token_id acc.storage.ledger)
      then (failwith "FA2_INVALID_TOKEN_ID" : minted1)
      else
        let new_token_metadata = 
          Big_map.add new_token_id t.token_metadata acc.storage.token_metadata in
        let next_token_id : nat = new_token_id + 1n in 
        let new_storage = { acc.storage with
          token_metadata = new_token_metadata;
          next_token_id = next_token_id;
        } in

        let tx : transfer_destination_descriptor = {
          to_ = Some t.owner;
          token_id = new_token_id;
          amount = 1n;
        } in

        {
          storage = new_storage;
          reversed_txs = tx :: acc.reversed_txs;
        }
    ) param seed1

let mint_tokens (param, storage : mint_tokens_param * nft_token_storage)
    : operation list * nft_token_storage =
  let mint1 = update_meta_and_create_txs (param, storage) in
  (* update ledger *)
  let tx_descriptor : transfer_descriptor = {
    from_ = (None : address option);
    txs = mint1.reversed_txs;
  } in
  let nop_operator_validator =
    fun (p : address * address * token_id * operator_storage) -> unit in
  let ops, storage = fa2_transfer ([tx_descriptor], nop_operator_validator, mint1.storage) in
  ops, storage

#else 

type mint_edition_param =
[@layout:comb]
{
  token_id: token_id;
  owner : address;
}

type mint_editions_param = mint_edition_param list

let create_txs_editions (param, storage : mint_editions_param * nft_token_storage) 
  : (transfer_destination_descriptor list) =
  let seed1 : minted1 = {
    storage = storage;
    reversed_txs = ([] : transfer_destination_descriptor list);
  } in
  let acc : minted1 = (List.fold
    (fun (acc, t : minted1 * mint_edition_param) ->
      let new_token_id = t.token_id in
      if (Big_map.mem new_token_id acc.storage.ledger)
      then (failwith "FA2_INVALID_TOKEN_ID" : minted1)
      else
        let tx : transfer_destination_descriptor = {
          to_ = Some t.owner;
          token_id = new_token_id;
          amount = 1n;
        } in
        {
          storage = acc.storage;
          reversed_txs = tx :: acc.reversed_txs; 
        }
    ) param seed1) in 
  (acc.reversed_txs)

let mint_edition_set (param, storage : mint_editions_param * nft_token_storage)
    : operation list * nft_token_storage =
  (* update ledger *)
  let tx_descriptor : transfer_descriptor = {
    from_ = (None : address option);
    txs = (create_txs_editions (param, storage));
  } in
  let nop_operator_validator =
    fun (p : address * address * token_id * operator_storage) -> unit in
  let ops, storage = fa2_transfer ([tx_descriptor], nop_operator_validator, storage) in
  ops, storage
#endif
#endif
