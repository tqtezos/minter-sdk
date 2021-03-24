(**
Definition of the minter module signature and implementation of the "null minter".
Null minter - nobody can mint
 *)

#if !NULL_MINTER_ADMIN
#define NULL_MINTER_ADMIN

type minter_admin_storage = (address, unit) big_map
type minter_admin_entrypoints =
  | Add_minter of address
  | Remove_minter of address

(* True if sender is a minter *)
[@inline]
let is_minter (storage : minter_admin_storage) : bool =
  Big_map.mem Tezos.sender storage


let minter_admin_main(param, storage : minter_admin_entrypoints * minter_admin_storage)
    : (operation list) * minter_admin_storage =
  match param with
  | Add_minter minter ->
    ([] : operation list), Big_map.add minter unit storage

  | Remove_minter minter ->
    ([] : operation list), Big_map.remove minter storage

#endif