#if !FA2_MULTI_NFT_TOKEN_WITH_ROYALTY

#define FA2_MULTI_NFT_TOKEN_WITH_ROYALTY

#include "../../fa2/fa2_interface.mligo"
#include "../../fa2/fa2_errors.mligo"

#include "../../fa2/lib/fa2_operator_lib.mligo"
#include "../../fa2/lib/fa2_owner_hooks_lib.mligo"
#include "../../math.mligo"
#include "fa2_multi_nft_token.mligo"

type royalty_data = 
[@layout:comb]
{
  creator : address; 
  fee_percent :  nat
}

type get_royalty_params =
[@layout:comb]
{
  token_id : token_id;
  fee : tez;
  cb : (address * tez) contract;
}

type fa2_royalty_entry_points = 
  | FA2 of fa2_entry_points
  | Get_Royalty of get_royalty_params

let royalty_undefined = "FA2_ROYALTY_UNDEFINED"

let getRoyalty (params, storage : get_royalty_params * nft_token_storage)
    : (operation list) * nft_token_storage =
  let royalty_data : royalty_data = ( match (Big_map.find_opt params.token_id storage.token_metadata) with 
    | Some md -> (match (Big_map.find_opt "royalty" md.token_info) with 
        | Some royalty_bytes -> ( match ((Bytes.unpack royalty_bytes) : royalty_data option ) with 
            | Some royalty_data -> royalty_data
            | None -> (failwith royalty_undefined : royalty_data))
        | None -> (failwith royalty_undefined : royalty_data))
    | None -> (failwith fa2_token_undefined : royalty_data)) in
  let royalty_fee = ceil_div(royalty_data.fee_percent * params.fee, 100n) in 
  let op = Tezos.transaction (royalty_data.creator, royalty_fee) 0mutez params.cb
  in [op], storage 

let fa2_main (param, storage : fa2_entry_points * nft_token_storage)
    : (operation  list) * nft_token_storage =
  match param with
  | Transfer txs ->
    let tx_descriptors = transfers_to_descriptors txs in
    let operator_transfer : operator_transfer_policy = Operator_transfer in
    let operator_validator = make_operator_validator operator_transfer in 
    (* will validate that a sender is an operator.*)
    fa2_transfer (tx_descriptors, operator_validator, storage)
  | Balance_of p ->
    let op = get_balance (p, storage.ledger) in
    [op], storage
  | Update_operators updates ->
    let u = (failwith fa2_operators_not_supported : (operation  list) * nft_token_storage)
    in u

let fa2_royalty_main (param, storage : fa2_royalty_entry_points * nft_token_storage)
  : (operation list) * nft_token_storage = match param with
   | FA2 entrypoints -> fa2_main(entrypoints, storage)
   | Get_Royalty royalty_param -> getRoyalty(royalty_param, storage)
    
#endif
