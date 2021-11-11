#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"
(* ==== Types ==== *)

type swap_id = nat

type set_id = nat

type fa2_token =
  [@layout:comb]
  { token_id : token_id
  ; amount : nat
  }
type fa2_assets =
  [@layout:comb]
  { fa2_address : address
  ; tokens : fa2_token list
  }

type swap_offer =
  [@layout:comb]
  { assets_offered : fa2_assets list
  ; assets_requested : set_id list
  }

type swap_offers = 
  [@layout:comb]
  {
    swap_offer : swap_offer;
    remaining_offers : nat;
  }  

type swap_info =
  [@layout:comb]
  { swap_offers : swap_offers
  ; seller : address
  } 

type set_info = token_id set (*Set data structure enforces uniqueness*)

type accept_param = 
  [@layout:comb]
  {  swap_id : swap_id 
  ;  tokens : (token_id, unit) map  (*Map enforces that each token in the set is unique, no duplicates.*)
  }

type swap_entrypoints =
  | Start of swap_offers
  | Cancel of swap_id
  | Accept of accept_param

type swap_storage =
  { next_swap_id : swap_id
  ; next_set_id : set_id
  ; swaps : (swap_id, swap_info) big_map
  ; sets : (set_id, set_info) big_map
  ; fa2_address : address
  }

type return = operation list * swap_storage

(* ==== Helpers ==== *)

(* This is just a marker of that the given error should happen
 * in no circumstances because of invariants of our contract.
 *)
[@inline]
let unexpected_err(err : string) : string = err

let forbid_xtz_transfer : unit = 
  let u : unit = assert_msg(Tezos.amount = 0tez, "XTZ_TRANSFER") in 
  u
 
let fa2_transfer_entrypoint(fa2, on_invalid_fa2 : address * string)
    : ((transfer list) contract) =
  match (Tezos.get_entrypoint_opt "%transfer" fa2 : (transfer list) contract option) with
  | None -> (failwith on_invalid_fa2 : (transfer list) contract)
  | Some c ->  c

let transfer_assets_uncurried(from_, to_, num_transfers, on_invalid_fa2, fa2_address : 
                    address * address * nat * string * address) (tokens : fa2_token list) 
                    : operation =
  let transfer_ep = fa2_transfer_entrypoint(fa2_address, on_invalid_fa2) in
  let token_to_tx_dest(token : fa2_token) : transfer_destination =
        { to_ = to_
        ; token_id = token.token_id
        ; amount = token.amount * num_transfers
        } in
  let param =
        { from_ = from_
        ; txs = List.map token_to_tx_dest tokens
        } in
  Tezos.transaction [param] 0mutez transfer_ep                     

let transfer_unique_assets(from_, to_, num_transfers, on_invalid_fa2, fa2_address : 
                    address * address * nat * string * address) (tokens : token_id list) 
                    : operation =
    let unique_token_to_fa2_token(token_id : token_id) : fa2_token = 
      { token_id = token_id 
      ; amount = 1n
      } in 
    let fa2_tokens = List.map unique_token_to_fa2_token tokens in 
    transfer_assets_uncurried(from_, to_, num_transfers, on_invalid_fa2, fa2_address)(fa2_tokens)

let transfer_assets(from_, to_, num_transfers, on_invalid_fa2 : address * address * nat * string)(asset : fa2_assets)
    : operation =
  (transfer_assets_uncurried(from_, to_, num_transfers, on_invalid_fa2, asset.fa2_address) asset.tokens)

[@inline]
let get_swap(id, storage : swap_id * swap_storage) : swap_info =
  match Big_map.find_opt id storage.swaps with
  | None -> (failwith "SWAP_NOT_EXIST" : swap_info)
  | Some s -> s

let authorize_seller(swap : swap_info) : unit =
  if swap.seller = Tezos.sender
  then unit
  else failwith "NOT_SWAP_SELLER"

(* ==== Entrypoints ==== *)

let start_swap(swap_offers, storage : swap_offers * swap_storage) : return = begin
    assert_msg(swap_offers.remaining_offers > 0n, "OFFERS_MUST_BE_NONZERO");
    let swap_id = storage.next_swap_id in
    let seller = Tezos.sender in
    let swap : swap_info =
      { swap_offers = swap_offers
      ; seller = seller
      } in
    let storage = { storage with
        next_swap_id = storage.next_swap_id + 1n
      ; swaps = Big_map.add swap_id swap storage.swaps
      } in
  
    let ops =
          List.map
          (transfer_assets(seller, Tezos.self_address, swap_offers.remaining_offers, "SWAP_OFFERED_FA2_INVALID"))
          swap_offers.swap_offer.assets_offered in
  
    (ops, storage)
  end

let cancel_swap(swap_id, storage : swap_id * swap_storage) : return = begin
  let swap = get_swap(swap_id, storage) in
  authorize_seller(swap);

  let storage = { storage with swaps = Big_map.remove swap_id storage.swaps } in

  let ops =
        List.map
        (transfer_assets(Tezos.self_address, swap.seller, swap.swap_offers.remaining_offers, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offers.swap_offer.assets_offered in

  (ops, storage)
  end

let accept_swap(accept_param, storage : accept_param * swap_storage) : return = begin
  let {swap_id = swap_id; 
       tokens = tokens;} = accept_param in 

  let token_list : token_id list = 
    Map.fold
    (fun (token_id_list, (token_id, _) : (token_id list) * (token_id * unit)) -> token_id :: token_id_list)
    tokens
    ([] : token_id list)
    in

  let swap = get_swap(swap_id, storage) in

  assert_msg(List.length token_list = List.length swap.swap_offers.swap_offer.assets_requested, "NUMBER_OF_TOKENS_SENT_MUST_EQUAL_NUMBER_REQUESTED");
  
  let storage = 
    if swap.swap_offers.remaining_offers > 1n 
    then {storage with 
            swaps = Big_map.update swap_id 
              (Some 
                { swap with 
                    swap_offers = 
                        {swap.swap_offers with 
                          remaining_offers = abs (swap.swap_offers.remaining_offers - 1n)
                        }
                }
              )
              storage.swaps
         }
    else { storage with swaps = Big_map.remove swap_id storage.swaps }
    in 

  let ops =
        List.map
        (transfer_assets(Tezos.self_address, Tezos.sender, 1n, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offers.swap_offer.assets_offered in
  
  
  let transfer = transfer_unique_assets(Tezos.sender, swap.seller, 1n, "SWAP_REQUESTED_FA2_INVALID", storage.fa2_address) in 
  let op : operation = transfer token_list in 
  let allOps = op :: ops in 
  
  (*How does this compare to using List.fold on token_list in terms of gas cost?*)
  let _ = ( Map.fold
            (fun (set_ids, (token, _) : (set_id list) * (token_id * unit)) -> 
                match set_ids with 
                  | [] -> (failwith "INTERNAL_ERROR" : set_id list)
                  | set_id :: remaining_ids -> 
                      let set : set_info = (match Big_map.find_opt set_id storage.sets with 
                                  | None -> (failwith "INVALID_SET_ID" : set_info)
                                  | Some set -> set 
                                ) in 
                      let u : unit = assert_msg(Set.mem token set, "INVALID_TOKEN") in 
                      remaining_ids  
            )
            tokens 
            swap.swap_offers.swap_offer.assets_requested      
          ) in
  (allOps, storage)
  end

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Start swap_offers -> start_swap(swap_offers, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept swap_id -> accept_swap(swap_id, storage)
  end
