#include "../common.mligo"
#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/simple_admin.mligo"
(* ==== Types ==== *)

type offchain_redeem_param = 
  [@layout:comb]
  {
    pack_token_id : nat;
    permit : permit;
  }

type pack_state = 
    Redeemed of address
  | Distributed of address

type booster_entrypoints =
    Redeem_booster of nat 
  | Offchain_redeem_booster of offchain_redeem_param 
  | Admin_distribute_packs of nat list 
  | Admin of admin_entrypoints

type booster_storage =
  [@layout:comb]
  { pack_fa2 : address
  ; burn_address : address
  ; packs : (nat, pack_state) big_map
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
let get_pack(id, storage : nat * booster_storage) : pack_state =
  match Big_map.find_opt id storage.packs with
  | None -> (failwith "PACK_NOT_EXIST" : pack_state)
  | Some bs -> bs

let global_token_id_to_tokens(id : global_token_id) : tokens = 
  ({
      fa2_address = id.fa2_address
    ; fa2_batch = [{
        token_id = id.token_id;
        amount = 1n;
    }];
  } : tokens)


(* ==== Entrypoints ==== *)

let redeem(pack_token_id, pack_owner, storage : nat * address * booster_storage) : return = begin 
  assert_msg(not Big_map.mem pack_token_id storage.packs, "BOOSTER_ALREADY_REDEEMED");
  let burn_pack_op : operation = 
      transfer_fa2(storage.pack_fa2, pack_token_id, 1n, pack_owner, storage.burn_address) in 
  let new_pack_registry = Big_map.add pack_token_id (Redeemed pack_owner : pack_state) storage.packs in 
  ([burn_pack_op], {storage with packs = new_pack_registry;})
end

(*

let add_single_pack (storage, token_id : booster_storage * nat) : booster_storage = begin 
    let next_pack_id = storage.next_pack_id in
    let new_packs_bm = Big_map.add next_pack_id (token_id, (Unredeemed : pack_state)) storage.packs in 
    {storage with packs = new_packs_bm; next_pack_id = next_pack_id + 1n}
  end

let add_packs(packs, storage : nat list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_left add_single_pack storage packs in 
    (([] : operation list), new_storage)
  end

*)

let redeem_with_permit (p, storage : offchain_redeem_param * booster_storage)  : return = begin 
    fail_if_not_admin(storage.admin);
    let {pack_token_id = pack_token_id;
         permit = permit; } = p in 
    let param_hash = Crypto.blake2b (Bytes.pack pack_token_id) in 
    let v : unit = check_permit (permit, 0n, param_hash) in  
    let redeemer = address_from_key (p.permit.signerKey) in
    let (ops, storage) = redeem(pack_token_id, redeemer, storage) in 
    (ops, storage)
  end

let distribute_pack (storage, pack_token_id : booster_storage * nat)  : booster_storage = begin 
    fail_if_not_admin(storage.admin);
    let pack_state : pack_state = 
        get_pack(pack_token_id, storage) in 
    let redeemer : address = match pack_state with 
        Redeemed redeemer -> redeemer 
      | Distributed -> (failwith "PACK_ALREADY_DISTRIBUTED" : address)
      in
    let new_pack_registry = Big_map.update pack_token_id (Some (Distributed redeemer : pack_state)) storage.packs in 
    {storage with packs = new_pack_registry;}
  end

let distribute_packs(packs, storage : nat list * booster_storage) : return = begin 
    fail_if_not_admin(storage.admin);
    let new_storage : booster_storage = List.fold_left distribute_pack storage packs in 
    (([] : operation list), new_storage)
  end


let booster_main (param, storage : booster_entrypoints * booster_storage) : return = begin
  forbid_xtz_transfer;
  match param with
    | Redeem_booster pack_id -> redeem(pack_id, Tezos.sender, storage)
    | Offchain_redeem_booster offchain_redeem_param -> redeem_with_permit(offchain_redeem_param, storage) 
    | Admin_distribute_packs pack_ids -> distribute_packs(pack_ids, storage) 
    | Admin admin_param ->
      (*admin_main already admin checks entrypoint*)
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      (ops, { storage with admin = admin_storage })
  end
