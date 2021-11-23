#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"
(* ==== Types ==== *)

type set_id = nat

type swap_id = nat

type fa2_token =
  [@layout:comb]
  { token_id : token_id
  ; amount : nat
  }

type tokens = fa2_token list 

type set_info = token_id set (*Set data structure enforces uniqueness of tokens in a given set*)

type swap_offer =
  [@layout:comb]
  { assets_offered : tokens
  ; assets_requested : set_id list (*Swap offerrer is expected to sort set_ids in descending order*)
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

type tokens_desc = (set_id * token_id) set (*Set data structure enforces uniqueness of set_id, token_id pairs
                                             Set also sorts in ascending order in expected way*) 

type accept_param = 
  [@layout:comb]
  {  swap_id : swap_id 
  ;  tokens : tokens_desc
  } 

type swap_entrypoints =
  | Start of swap_offers
  | Cancel of swap_id
  | Accept of accept_param
  | Add_set of set_info

type swap_storage =
  { next_swap_id : swap_id
  ; next_set_id : set_id
  ; swaps : (swap_id, swap_info) big_map
  ; burn_address : address
  ; sets : (set_id, set_info) big_map
  ; fa2_address : address (*Every token this contract interacts with must be defined in this FA2*)
  }

type return = operation list * swap_storage

(* ==== Values ==== *)

[@inline] let example_burn_address = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)

[@inline] let example_fa2_address = ("KT1KUJkqqgfe1AaJMdRfspsyt5diWgYm3eGM" : address) 

let init_storage : swap_storage =
  { burn_address = example_burn_address
  ; next_swap_id = 0n
  ; next_set_id = 0n
  ; swaps = (Big_map.empty : (swap_id, swap_info) big_map)
  ; sets = (Big_map.empty : (set_id, set_info) big_map)
  ; fa2_address = example_fa2_address
  }

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

let transfer_tokens_uncurried(from_, to_, num_transfers, on_invalid_fa2, fa2_address : 
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

let transfer_single_tokens(from_, to_, num_transfers, on_invalid_fa2, fa2_address : 
                    address * address * nat * string * address) (tokens : token_id list) 
                    : operation =
    let unique_token_to_fa2_token(token_id : token_id) : fa2_token = 
      { token_id = token_id 
      ; amount = 1n
      } in 
    let fa2_tokens = List.map unique_token_to_fa2_token tokens in 
    transfer_tokens_uncurried(from_, to_, num_transfers, on_invalid_fa2, fa2_address)(fa2_tokens)

let transfer_tokens(from_, to_, num_transfers, fa2_address, on_invalid_fa2 : address * address * nat * address * string)(tokens : tokens)
    : operation =
  (transfer_tokens_uncurried(from_, to_, num_transfers, on_invalid_fa2, fa2_address) tokens)

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
  
    let op =
          (transfer_tokens(seller, Tezos.self_address, swap_offers.remaining_offers, storage.fa2_address, "SWAP_OFFERED_FA2_INVALID"))
          swap_offers.swap_offer.assets_offered in
  
    ([op], storage)
  end

let cancel_swap(swap_id, storage : swap_id * swap_storage) : return = begin
  let swap = get_swap(swap_id, storage) in
  authorize_seller(swap);

  let storage = { storage with swaps = Big_map.remove swap_id storage.swaps } in

  let op =
        (transfer_tokens(Tezos.self_address, swap.seller, swap.swap_offers.remaining_offers, storage.fa2_address, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offers.swap_offer.assets_offered in

  ([op], storage)
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

let accept_swap_update_ops_list(swap, tokens, accepter, bid_offchain, ops, storage : swap_info * tokens_desc * address * bool * operation list * swap_storage) : operation list = begin

  (*Converts the token set to a list of token_ids, reverses order so that they line up with the assets requested set_id order*) 
  let token_list : token_id list = 
    Set.fold
    (fun (token_id_list, (_ , token_id) : (token_id list) * (set_id * token_id)) -> token_id :: token_id_list)
    tokens
    ([] : token_id list)
    in
  
  (*Asserts that the number of tokens sent equals the number requested in the swap*)
  assert_msg(List.length token_list = List.length swap.swap_offers.swap_offer.assets_requested, "NUMBER_OF_TOKENS_SENT_MUST_EQUAL_NUMBER_REQUESTED");
  
  (*Tests that tokens provided are in the required sets*)
  let u = ( List.fold
            (fun (set_ids, token : (set_id list) * token_id ) -> 
                match set_ids with 
                  | [] -> (failwith (unexpected_err("INTERNAL_ERROR")) : set_id list)
                  | set_id :: remaining_ids -> 
                      let set : set_info = (match Big_map.find_opt set_id storage.sets with 
                                  | None -> (failwith "INVALID_SET_ID" : set_info)
                                  | Some set -> set 
                                ) in 
                      let u : unit = assert_msg(Set.mem token set, "INVALID_TOKEN") in 
                      remaining_ids  
            )
            token_list
            swap.swap_offers.swap_offer.assets_requested      
          ) in

  (*Transferring the offered tokens*)
  let transfer_offered_op =
        (transfer_tokens(Tezos.self_address, Tezos.sender, 1n, storage.fa2_address, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offers.swap_offer.assets_offered in
  
  (*Transferring the requested tokens*)
  let transfer = transfer_single_tokens(Tezos.sender, storage.burn_address, 1n, "SWAP_REQUESTED_FA2_INVALID", storage.fa2_address) in 
  let transfer_requested_op : operation = transfer token_list in 
  [transfer_offered_op; transfer_requested_op;]
 end

let accept_swap(accept_param, accepter, storage : accept_param * address * swap_storage) : return = 
    let {swap_id = swap_id ;
         tokens = tokens} = accept_param in 
    let swap = get_swap(swap_id, storage) in
    let bid_offchain = false in 
    let ops = accept_swap_update_ops_list(swap, tokens, accepter, bid_offchain, ([] : operation list), storage) in
    let storage = accept_swap_update_storage(swap_id, swap, accepter, storage) in
    (ops, storage)

let add_set(set_info, storage : set_info * swap_storage) : return = begin 
    let next_set_id = storage.next_set_id in
    let new_sets_bm = Big_map.add next_set_id set_info storage.sets in 
    (([]: operation list), {storage with sets = new_sets_bm; next_set_id = next_set_id + 1n})
  end

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Start swap_offers -> start_swap(swap_offers, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept swap_id -> accept_swap(swap_id, Tezos.sender, storage)
    | Add_set set_info -> add_set(set_info, storage)
  end
