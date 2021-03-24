(**
Definition of the minter module signature and implementation of the "not minter".
Not minter - nobody can mint
 *)

#if !NOT_MINTER_ADMIN
#define NOT_MINTER_ADMIN

type minter_admin_storage = unit
type minter_admin_entrypoints = never

(* True if sender is a minter *)
[@inline]
let is_minter (storage : minter_admin_storage) : bool = false


let minter_admin_main(param, storage : minter_admin_entrypoints * minter_admin_storage)
    : (operation list) * minter_admin_storage =
  ([] : operation list), storage

#endif