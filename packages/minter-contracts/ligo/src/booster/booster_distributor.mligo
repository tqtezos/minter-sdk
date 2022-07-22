#include "../common.mligo"
#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/simple_admin.mligo"
(* ==== Types ==== *)

type pack_id = nat

type token_registry_id = nat

type redeem_param = 
  [@layout:comb]
  {
    pack_id : nat;
    tokens_contained : nat list;
    nonce : nat; 
  }

type booster_entrypoints =
    Add_packs of (global_token_id * bytes) list
  | Add_tokens of global_token_id list
  | Redeem_booster of redeem_param 
  | Admin of admin_entrypoints

type booster_storage =
  [@layout:comb]
  { next_pack_id : pack_id
  ; next_token_registry_id : token_registry_id 
  ; packs : (pack_id, global_token_id * bytes) big_map
  ; token_registry : (nat, global_token_id) big_map
  ; admin : admin_storage
  }

type return = operation list * booster_storage

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

let transfer_tokens_in_single_contract (from_ : address) (to_ : address) (tokens : tokens) : operation =
  let to_tx (fa2_tokens : fa2_tokens) : transfer_destination = {
      to_ = to_;
      token_id = fa2_tokens.token_id;
      amount = fa2_tokens.amount;
   } in
   let txs = List.map to_tx tokens.fa2_batch in
   let transfer_param = [{from_ = from_; txs = txs}] in
   let c = address_to_contract_transfer_entrypoint(tokens.fa2_address) in
   (Tezos.transaction transfer_param 0mutez c)

(*Handles transfers of tokens across FA2 Contracts*)
let transfer_tokens(tokens_list, from_, to_ : tokens list * address * address) : (operation list) =
   (List.map (transfer_tokens_in_single_contract from_ to_) tokens_list)
 
[@inline]
let get_pack(id, storage : pack_id * booster_storage) : global_token_id * bytes =
  match Big_map.find_opt id storage.packs with
  | None -> (failwith "PACK_NOT_EXIST" : global_token_id * bytes)
  | Some bs -> bs

[@inline]
let get_token(id, storage : nat * booster_storage) : global_token_id =
  match Big_map.find_opt id storage.token_registry with
  | None -> (failwith "TOKEN_NOT_IN_REGISTRY" : global_token_id)
  | Some token -> token

let global_token_id_to_tokens(id : global_token_id) : tokens = 
  ({
      fa2_address = id.fa2_address
    ; fa2_batch = [{
        token_id = id.token_id;
        amount = 1n;
    }];
  } : tokens)


(* ==== Entrypoints ==== *)

let transfer_pack_contents_to_redeemer(tokens_contained, storage : nat list * booster_storage) : operation list = begin 
  let tokens_list = List.map 
                    (fun(token_registry_id : nat) -> 
                      let token : global_token_id = get_token(token_registry_id, storage) in 
                      let token_to_transfer : tokens = global_token_id_to_tokens(token) in 
                      token_to_transfer
                    ) 
                    tokens_contained
                    in 
  let transfer_ops = transfer_tokens(tokens_list, Tezos.self_address, Tezos.sender) in 
  transfer_ops
                    
end

let redeem(redeem_param, storage : redeem_param * booster_storage) : return = begin 
  let { pack_id = pack_id;
        tokens_contained = tokens_contained;
        nonce = nonce;
      } = redeem_param in 
  let (pack_token_data, booster_pack_bytes) : global_token_id * byes = 
      get_pack(pack_id, storage) in 
  let transfer_pack_to_self_op : operation = 
      transfer_fa2(pack_token_data.fa2_address, pack_token_data.token_id, 1n, Tezos.sender, Tezos.self_address) in 
  let hashed_data : bytes = Crypto.sha256 (Bytes.pack redeem_param) in
  assert_msg(hashed_data = booster_pack_bytes, "HASHES_DONT_MATCH");
  let ops : operation list = transfer_pack_contents_to_redeemer(tokens_contained, storage) in 
  let new_pack_registry = Big_map.remove pack_id storage.packs in 
  (transfer_pack_to_self_op :: ops, {storage with packs = new_pack_registry;})
end

let add_single_pack (pack_data, storage : (global_token_id * bytes) * booster_storage) : booster_storage = begin 
    let next_pack_id = storage.next_pack_id in
    let new_packs_bm = Big_map.add next_pack_id pack_data storage.packs in 
    {storage with packs = new_packs_bm; next_pack_id = next_pack_id + 1n}
  end

let add_packs(packs, storage : (global_token_id * bytes) list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_right add_single_pack packs storage in 
    (([] : operation list), new_storage)
  end

let add_single_token (token, storage : global_token_id * booster_storage) : booster_storage = begin 
    let next_token_registry_id = storage.next_token_registry_id in
    let new_token_registry_bm = Big_map.add next_token_registry_id token storage.token_registry in 
    {storage with token_registry = new_token_registry_bm; next_token_registry_id = next_token_registry_id + 1n}
  end

let add_tokens(token_list, storage : global_token_id list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_right add_single_token token_list storage in 
    (([] : operation list), new_storage)
  end

let booster_main (param, storage : booster_entrypoints * booster_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Add_packs pack_list -> add_packs(pack_list, storage)
    | Add_tokens token_list -> add_tokens(token_list, storage)
    | Redeem_booster redeem_param -> redeem(redeem_param, storage)
    | Admin admin_param ->
      (*admin_main already admin checks entrypoint*)
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      (ops, { storage with admin = admin_storage })
  end
