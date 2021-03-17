#if !SIMPLE_ADMIN
#define SIMPLE_ADMIN

type admin_storage = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}
type admin_entrypoints =
  | Set_admin of address
  | Confirm_admin of unit
  | Pause of bool

let confirm_new_admin (storage : admin_storage) : admin_storage =
  match storage.pending_admin with
  | None -> (failwith "NO_PENDING_ADMIN" : admin_storage)
  | Some pending ->
    if Tezos.sender = pending
    then { storage with
      pending_admin = (None : address option);
      admin = Tezos.sender;
    }
    else (failwith "NOT_A_PENDING_ADMIN" : admin_storage)
  
(* Fails if sender is not admin *)
let fail_if_not_admin (storage : admin_storage) (extra_msg : string option) : unit =
  if Tezos.sender <> storage.admin
  then match extra_msg with
  | None -> failwith "NOT_AN_ADMIN"
  | Some msg -> failwith ("NOT_AN_ADMIN" ^  " "  ^ msg)
  else unit

(* Returns true if sender is admin *)
let is_admin (storage : admin_storage) : bool = Tezos.sender = storage.admin

let fail_if_paused (storage : admin_storage) : unit =
  if(storage.paused)
  then failwith "PAUSED"
  else unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with pending_admin = Some new_admin; }
    
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

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)

  | Pause paused ->
      let new_s = pause (paused, storage) in
      (([]: operation list), new_s)

#endif