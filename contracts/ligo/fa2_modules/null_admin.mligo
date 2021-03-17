(**
Definition of the ADMIN module signature and implementation of the "null admin".
Null admin - everyone is an admin

 *)

#if !NULL_ADMIN
#define NULL_ADMIN

type admin_storage = unit
type admin_entrypoints = never

(* Fails if sender is not admin*)
let fail_if_not_admin (storage : admin_storage) (extra_msg : string option) : unit = unit

(* Returns true if sender is admin *)
let is_admin (storage : admin_storage) : bool = true


let fail_if_paused (storage : admin_storage) : unit = unit

let admin_main(param, storage : admin_entrypoints * admin_storage)
    : (operation list) * admin_storage = ([] : operation list), s

#endif


