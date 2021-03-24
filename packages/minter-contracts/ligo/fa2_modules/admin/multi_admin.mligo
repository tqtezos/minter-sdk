#if !MULTI_ADMIN
#define MULTI_ADMIN

type admin_storage = {
  admins : address set; (* using set to detect if we try to remove last admin*)
  pending_admins : (address, unit) big_map;
  paused : bool;
}
type admin_entrypoints =
  | Set_admin of address
  | Remove_admin of address
  | Confirm_admin of unit
  | Pause of bool

let confirm_new_admin (storage : admin_storage) : admin_storage =
  if Big_map.mem Tezos.sender storage.pending_admins
  then 
  { storage with
    admins = Set.add Tezos.sender storage.admins;
    pending_admins = Big_map.remove Tezos.sender storage.pending_admins;
  }
  else (failwith "NOT_A_PENDING_ADMIN" : admin_storage)

(* Fails if sender is not admin *)
let fail_if_not_admin_ext (storage, extra_msg : admin_storage * string) : unit =
  if not Set.mem Tezos.sender storage.admins
  then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
  else unit

(* Fails if sender is not admin *)
let fail_if_not_admin (storage : admin_storage) : unit =
  if not Set.mem Tezos.sender storage.admins
  then failwith "NOT_AN_ADMIN"
  else unit

(* Returns true if sender is admin *)
[@inline]
let is_admin (storage : admin_storage) : bool =
  Set.mem Tezos.sender storage.admins

[@inline]
let fail_if_paused (storage : admin_storage) : unit =
  if(storage.paused)
  then failwith "PAUSED"
  else unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with 
    pending_admins = Big_map.add new_admin unit storage.pending_admins; 
  }

(*Only callable by admin*)
let remove_admin (old_admin, storage : address * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  if(Set.size storage.admins = 1n)
  then (failwith "LAST_ADMIN" : admin_storage)
  else { storage with admins = Set.remove old_admin storage.admins; }
    
(*Only callable by admin*)
let pause (paused, storage: bool * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with paused = paused; }

let admin_main(param, storage : admin_entrypoints * admin_storage)
    : (operation list) * admin_storage =
  match param with
  | Set_admin new_admin ->
      let new_s = set_admin (new_admin, storage) in
      (([] : operation list), new_s)

  | Remove_admin old_admin ->
      let new_s = remove_admin (old_admin, storage) in
      (([] : operation list), new_s)

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)

  | Pause paused ->
      let new_s = pause (paused, storage) in
      (([]: operation list), new_s)

#endif