#include "../../fa2/fa2_interface.mligo"

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
  ; assets_requested : fa2_assets list
  }

type swap_info =
  [@layout:comb]
  { swap_offer : swap_offer
  ; seller : address
  }

type swap_entrypoints =
  | Start of swap_offer
  | Cancel of swap_id
  | Accept of swap_id

type swap_storage =
  { next_swap_id : swap_id
  ; swaps : (swap_id, swap_info) big_map
  }

type return = operation list * swap_storage

(* ==== Values ==== *)

let init_storage : swap_storage =
  { next_swap_id = 0n
  ; swaps = (Big_map.empty : (swap_id, swap_info) big_map)
  }

(* ==== Helpers ==== *)

(* This is just a marker of that the given error should happen
 * in no circumstances because of invariants of our contract.
 *)
[@inline]
let unexpected_err(err : string) : string = err

let forbid_xtz_transfer : unit =
  if Tezos.amount = 0tez
  then unit
  else failwith "XTZ_TRANSFER"

let fa2_transfer_entrypoint(fa2, on_invalid_fa2 : address * string)
    : ((transfer list) contract) =
  match (Tezos.get_entrypoint_opt "%transfer" fa2 : (transfer list) contract option) with
  | None -> (failwith on_invalid_fa2 : (transfer list) contract)
  | Some c ->  c

let transfer_asset(from_, to_, on_invalid_fa2 : address * address * string)(asset : fa2_assets)
    : operation =
  let transfer_ep = fa2_transfer_entrypoint(asset.fa2_address, on_invalid_fa2) in
  let token_to_tx_dest(token : fa2_token) : transfer_destination =
        { to_ = to_
        ; token_id = token.token_id
        ; amount = token.amount
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

let start_swap(swap_offer, storage : swap_offer * swap_storage) : return =
  let swap_id = storage.next_swap_id in
  let seller = Tezos.sender in
  let swap : swap_info =
    { swap_offer = swap_offer
    ; seller = seller
    } in
  let storage = { storage with
      next_swap_id = storage.next_swap_id + 1n
    ; swaps = Big_map.add swap_id swap storage.swaps
    } in

  let ops =
        List.map
        (transfer_asset(seller, Tezos.self_address, "SWAP_OFFERED_FA2_INVALID"))
        swap_offer.assets_offered in

  ops, storage

let cancel_swap(swap_id, storage : swap_id * swap_storage) : return = begin
  let swap = get_swap(swap_id, storage) in
  authorize_seller(swap);

  let storage = { storage with swaps = Big_map.remove swap_id storage.swaps } in

  let ops =
        List.map
        (transfer_asset(Tezos.self_address, swap.seller, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offer.assets_offered in

  (ops, storage)
  end

let accept_swap(swap_id, storage : swap_id * swap_storage) : return =
  let swap = get_swap(swap_id, storage) in

  let storage = { storage with swaps = Big_map.remove swap_id storage.swaps } in

  let ops1 =
        List.map
        (transfer_asset(Tezos.self_address, Tezos.sender, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offer.assets_offered in
  let ops2 =
        List.map
        (transfer_asset(Tezos.sender, swap.seller, "SWAP_REQUESTED_FA2_INVALID"))
        swap.swap_offer.assets_requested in
  let snoc_ops (l, a : operation list * operation) = a :: l in
  let allOps = List.fold snoc_ops ops1 ops2 in

  allOps, storage

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Start swap_offer -> start_swap(swap_offer, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept swap_id -> accept_swap(swap_id, storage)
  end
