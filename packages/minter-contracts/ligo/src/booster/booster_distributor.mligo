#include "../common.mligo"
#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/simple_admin.mligo"
(* ==== Types ==== *)

type pack_id = nat

type redeem_key = 
  [@layout:comb]
  {
    pack_id : nat;
    tokens_contained : nat list;
    nonce : nat; 
  }

type offchain_redeem_param = 
  [@layout:comb]
  {
    redeem_key : redeem_key;
    permit : permit;
  }

type booster_entrypoints =
    Add_packs of (global_token_id * bytes) list
  | Add_tokens of global_token_id list
  | Redeem_booster of redeem_key
  | Offchain_redeem_booster of offchain_redeem_param
  | Admin of admin_entrypoints

type booster_storage =
  [@layout:comb]
  { next_pack_id : pack_id
  ; next_token_registry_id : nat
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

let transfer_pack_contents_to_redeemer(tokens_contained, pack_owner, storage : nat list * address * booster_storage) : operation list = begin 
  let tokens_list = List.map 
                    (fun(token_registry_id : nat) -> 
                      let token : global_token_id = get_token(token_registry_id, storage) in 
                      let token_to_transfer : tokens = global_token_id_to_tokens(token) in 
                      token_to_transfer
                    ) 
                    tokens_contained
                    in 
  let transfer_ops = transfer_tokens(tokens_list, storage.admin.admin, pack_owner) in 
  transfer_ops
                    
end

let redeem(redeem_key, pack_owner, storage : redeem_key * address * booster_storage) : return = begin 
  let  { 
         pack_id = pack_id;
         tokens_contained = tokens_contained;
         nonce = nonce;
       } = redeem_key in 
  let (pack_token_data, booster_pack_bytes) : global_token_id * byes = 
      get_pack(pack_id, storage) in 
  let transfer_pack_to_self_op : operation = 
      transfer_fa2(pack_token_data.fa2_address, pack_token_data.token_id, 1n, pack_owner, storage.admin.admin) in 
  let hashed_data : bytes = Crypto.blake2b (Bytes.pack (pack_id, (tokens_contained, nonce))) in
  let u : unit = (if hashed_data <> booster_pack_bytes
     then ([%Michelson ({| { FAILWITH } |} : string * bytes * bytes -> unit)] ("HASHES_DONT_MATCH", hashed_data, booster_pack_bytes) : unit)
     else ()) in 
  let ops : operation list = transfer_pack_contents_to_redeemer(tokens_contained, pack_owner, storage) in 
  let new_pack_registry = Big_map.remove pack_id storage.packs in 
  (transfer_pack_to_self_op :: ops, {storage with packs = new_pack_registry;})
end

let add_single_pack (storage, pack_data : booster_storage * (global_token_id * bytes)) : booster_storage = begin 
    let next_pack_id = storage.next_pack_id in
    let new_packs_bm = Big_map.add next_pack_id pack_data storage.packs in 
    {storage with packs = new_packs_bm; next_pack_id = next_pack_id + 1n}
  end

let add_packs(packs, storage : (global_token_id * bytes) list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_left add_single_pack storage packs in 
    (([] : operation list), new_storage)
  end

let add_single_token (storage, token : booster_storage *  global_token_id) : booster_storage = begin 
    let next_token_registry_id = storage.next_token_registry_id in
    let new_token_registry_bm = Big_map.add next_token_registry_id token storage.token_registry in 
    {storage with token_registry = new_token_registry_bm; next_token_registry_id = next_token_registry_id + 1n}
  end

let add_tokens(token_list, storage : global_token_id list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_left add_single_token storage token_list in 
    (([] : operation list), new_storage)
  end

let redeem_with_permit (p, storage : offchain_redeem_param * booster_storage)  : return = begin 
    fail_if_not_admin(storage.admin);
    let {redeem_key = redeem_key;
         permit = permit; } = p in 
    let param_hash = Crypto.blake2b (Bytes.pack redeem_key) in 
    let v : unit = check_permit (permit, 0n, param_hash) in  (*Always set counter to 0*)
    let redeemer = address_from_key (p.permit.signerKey) in
    let (ops, storage) = redeem(redeem_key, redeemer, storage) in 
    (ops, storage)
  end

let booster_main (param, storage : booster_entrypoints * booster_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Add_packs pack_list -> add_packs(pack_list, storage)
    | Add_tokens token_list -> add_tokens(token_list, storage)
    | Redeem_booster redeem_key -> redeem(redeem_key, Tezos.sender, storage)
    | Offchain_redeem_booster offchain_redeem_param -> redeem_with_permit(offchain_redeem_param, storage)
    | Admin admin_param ->
      (*admin_main already admin checks entrypoint*)
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      (ops, { storage with admin = admin_storage })
  end