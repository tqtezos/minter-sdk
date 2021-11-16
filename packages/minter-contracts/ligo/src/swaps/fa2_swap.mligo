#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"
(* ==== Types ==== *)

type swap_id = nat

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
#if !XTZ_FEE
  ; assets_requested : fa2_assets list
#else 
  ; assets_requested : fa2_assets list * tez
#endif
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

type swap_entrypoints =
  | Start of swap_offers
  | Cancel of swap_id
  | Accept of swap_id

type swap_storage =
  { next_swap_id : swap_id
  ; swaps : (swap_id, swap_info) big_map
#if BURN_PAYMENT
  ; burn_address : address
#endif
  }

type return = operation list * swap_storage

(* ==== Values ==== *)

[@inline] let example_burn_address = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)

let init_storage : swap_storage =
  { next_swap_id = 0n
  ; swaps = (Big_map.empty : (swap_id, swap_info) big_map)
#if BURN_PAYMENT
  ; burn_address = example_burn_address
#endif
  }

(* ==== Helpers ==== *)

(* This is just a marker of that the given error should happen
 * in no circumstances because of invariants of our contract.
 *)
[@inline]
let unexpected_err(err : string) : string = err

let forbid_xtz_transfer : unit = 
#if !XTZ_FEE
  let u : unit = assert_msg(Tezos.amount = 0tez, "XTZ_TRANSFER") in 
#else
  let u : unit = unit in 
#endif 
  u
 
let fa2_transfer_entrypoint(fa2, on_invalid_fa2 : address * string)
    : ((transfer list) contract) =
  match (Tezos.get_entrypoint_opt "%transfer" fa2 : (transfer list) contract option) with
  | None -> (failwith on_invalid_fa2 : (transfer list) contract)
  | Some c ->  c

let transfer_assets(from_, to_, num_transfers, on_invalid_fa2 : address * address * nat * string)(asset : fa2_assets)
    : operation =
  let transfer_ep = fa2_transfer_entrypoint(asset.fa2_address, on_invalid_fa2) in
  let token_to_tx_dest(token : fa2_token) : transfer_destination =
        { to_ = to_
        ; token_id = token.token_id
        ; amount = token.amount * num_transfers
        } in
  let param =
        { from_ = from_
        ; txs = List.map token_to_tx_dest asset.tokens
        } in
  Tezos.transaction [param] 0mutez transfer_ep

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

let accept_swap_update_storage(swap_id, swap, accepter, storage : swap_id * swap_info * address * swap_storage) : swap_storage = 
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
  storage

let accept_swap_update_ops_list(swap, accepter, ops, storage : swap_info * address * operation list * swap_storage) : operation list = begin
  let ops =
        List.fold
        (fun (ops, tokens : operation list * fa2_assets) ->
          let transfer = (transfer_assets(Tezos.self_address, accepter, 1n, unexpected_err "SWAP_OFFERED_FA2_INVALID")) in 
          let op : operation = transfer tokens in 
          (op :: ops)
        )
        swap.swap_offers.swap_offer.assets_offered 
        ops 
        in
  let allOps =
        List.fold
        (fun (ops, tokens : operation list * fa2_assets) ->  
#if !BURN_PAYMENT
          let transfer = (transfer_assets(accepter, swap.seller, 1n, "SWAP_REQUESTED_FA2_INVALID")) in 
#else 
          let transfer = (transfer_assets(accepter, storage.burn_address, 1n, "SWAP_REQUESTED_FA2_INVALID")) in
#endif
          let op : operation = transfer tokens in 
          (op :: ops)
        )
#if !XTZ_FEE
        swap.swap_offers.swap_offer.assets_requested 
#else
        swap.swap_offers.swap_offer.assets_requested.0
#endif
        ops 
        in 

#if XTZ_FEE 
  let xtz_requested : tez = swap.swap_offers.swap_offer.assets_requested.1 in 
  assert_msg(Tezos.amount = xtz_requested, "SWAP_REQUESTED_XTZ_INVALID");
  let allOps = 
    if xtz_requested = 0mutez 
    then allOps 
    else 
      let xtz_op = transfer_tez(xtz_requested, swap.seller) in  
      xtz_op :: allOps 
    in 
#endif
  allOps
 end

let accept_swap(swap_id, accepter, storage : swap_id * address * swap_storage) : return = 
    let swap = get_swap(swap_id, storage) in
    let storage = accept_swap_update_storage(swap_id, swap, accepter, storage) in
    let ops = accept_swap_update_ops_list(swap, accepter, ([] : operation list), storage) in
    (ops, storage)

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Start swap_offers -> start_swap(swap_offers, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept swap_id -> accept_swap(swap_id, Tezos.sender, storage)
  end
