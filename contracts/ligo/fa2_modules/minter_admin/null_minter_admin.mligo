(**
Definition of the minter module signature and implementation of the "null minter".
Null minter - everyone can mint
 *)

#if !NULL_MINTER_ADMIN
#define NULL_MINTER_ADMIN

type minter_admin_storage = unit
type minter_admin_entrypoints = never

(* True if sender is a minter *)
[@inline]
let is_minter (storage : minter_admin_storage) : bool = true


let minter_admin_main(param, storage : minter_admin_entrypoints * minter_admin_storage)
    : (operation list) * minter_admin_storage =
  ([] : operation list), storage

#endif