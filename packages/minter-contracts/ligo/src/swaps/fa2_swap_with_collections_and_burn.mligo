#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/non_pausable_simple_admin.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"
(* ==== Types ==== *)

type collection_id = nat

type swap_id = nat

type fa2_token =
  [@layout:comb]
  { token_id : token_id
  ; amount : nat
  }

type tokens = fa2_token list 

type collection_info = token_id set (*Set data structure enforces uniqueness of tokens in a given set*)

type swap_offer =
  [@layout:comb]
  { assets_offered : tokens
  ; assets_requested : collection_id list (*Swap offerrer is expected to sort set_ids in ascending order*)
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

type tokens_sent = (collection_id * token_id) set (*Set data structure enforces uniqueness of collection_id, token_id pairs
                                                          Set also sorts in ascending order in expected way*) 

type accept_param = 
  [@layout:comb]
  {  swap_id : swap_id 
  ;  tokens : tokens_sent
  } 

type swap_entrypoints =
  | Start of swap_offers
  | Cancel of swap_id
  | Accept of accept_param
  | Add_collection of collection_info
  | Admin of admin_entrypoints

type swap_storage =
  { next_swap_id : swap_id
  ; next_collection_id : collection_id
  ; swaps : (swap_id, swap_info) big_map
  ; burn_address : address
  ; collections : (collection_id, collection_info) big_map
  ; fa2_address : address (*Every token this contract interacts with must be defined in this FA2*)
  ; admin : admin_storage
  }

type return = operation list * swap_storage

(* ==== Values ==== *)

[@inline] let example_burn_address = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)

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

let transfer_tokens(from_, to_, num_transfers, fa2_address, on_invalid_fa2, tokens : address * address * nat * address * string * tokens)
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

let transfer_tokens_sent(from_, to_, num_transfers, fa2_address, on_invalid_fa2, tokens : 
                    address * address * nat * address * string * tokens_sent) 
                    : operation =
  let unique_token_to_fa2_token(token_id : token_id) : fa2_token = 
    { token_id = token_id 
    ; amount = 1n
    } in 
  let fa2_tokens = Set.fold 
                   ( fun (token_list, (_, token_id) : tokens * (collection_id * token_id)) -> unique_token_to_fa2_token(token_id) :: token_list) 
                   tokens 
                   ([] : tokens)
                   in 
  transfer_tokens(from_, to_, num_transfers, fa2_address, on_invalid_fa2, fa2_tokens)


[@inline]
let get_swap(id, storage : swap_id * swap_storage) : swap_info =
  match Big_map.find_opt id storage.swaps with
  | None -> (failwith "SWAP_NOT_EXIST" : swap_info)
  | Some s -> s

(* ==== Entrypoints ==== *)

let start_swap(swap_offers, storage : swap_offers * swap_storage) : return = begin
    fail_if_not_admin(storage.admin);
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
          transfer_tokens(seller, Tezos.self_address, swap_offers.remaining_offers, storage.fa2_address, "SWAP_OFFERED_FA2_INVALID", swap_offers.swap_offer.assets_offered) in
  
    ([op], storage)
  end

let cancel_swap(swap_id, storage : swap_id * swap_storage) : return = begin
  fail_if_not_admin(storage.admin);
  let swap = get_swap(swap_id, storage) in

  let storage = { storage with swaps = Big_map.remove swap_id storage.swaps } in

  let op =
        transfer_tokens(Tezos.self_address, swap.seller, swap.swap_offers.remaining_offers, storage.fa2_address, unexpected_err "SWAP_OFFERED_FA2_INVALID", swap.swap_offers.swap_offer.assets_offered) in

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

let accept_swap_update_ops_list(swap, tokens, accepter, bid_offchain, ops, storage : swap_info * tokens_sent * address * bool * operation list * swap_storage) : operation list = begin

  (*Tests that tokens provided are in the required collections*)
  let remaining_ids : (collection_id list)= 
         ( Set.fold
            (fun (set_ids, (_, token) : (collection_id list) * (collection_id * token_id) ) -> 
                match set_ids with 
                  | [] -> (failwith ("TOKENS_SENT_INVALID") : collection_id list)
                  | collection_id :: remaining_ids -> 
                      let collection : collection_info = (match Big_map.find_opt collection_id storage.collections with 
                                  | None -> (failwith "INVALID_SET_ID" : collection_info)
                                  | Some collection -> collection 
                                ) in 
                      let u : unit = assert_msg(Set.mem token collection, "INVALID_TOKEN") in 
                      remaining_ids  
            )
            tokens
            swap.swap_offers.swap_offer.assets_requested      
          ) in
  
  assert_msg(List.length remaining_ids = 0n, "TOKENS_SENT_INVALID");

  (*Transferring the offered tokens*)
  let transfer_offered_op =
        transfer_tokens(Tezos.self_address, accepter, 1n, storage.fa2_address, unexpected_err("SWAP_OFFERED_FA2_INVALID"), swap.swap_offers.swap_offer.assets_offered) in
  
  (*Transferring the requested tokens*)
  let transfer_requested_op = transfer_tokens_sent(accepter, storage.burn_address, 1n, storage.fa2_address, "SWAP_REQUESTED_FA2_INVALID", tokens) in 
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

let add_collection(collection_info, storage : collection_info * swap_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let next_collection_id = storage.next_collection_id in
    let new_collections_bm = Big_map.add next_collection_id collection_info storage.collections in 
    (([]: operation list), {storage with collections = new_collections_bm; next_collection_id = next_collection_id + 1n})
  end

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Start swap_offers -> start_swap(swap_offers, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept accept_param -> accept_swap(accept_param, Tezos.sender, storage)
    | Add_collection collection_info -> add_collection(collection_info, storage)
    | Admin admin_param ->
      (*admin_main already admin checks entrypoint*)
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      (ops, { storage with admin = admin_storage })
  end
