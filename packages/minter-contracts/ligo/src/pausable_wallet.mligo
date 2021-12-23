#include "../fa2_modules/admin/simple_admin.mligo"
#include "common.mligo"

type wallet_storage = admin_storage

type wallet_entrypoints =
  | Send of tez
  | Default of unit
  | Admin of admin_entrypoints

type return = operation list * wallet_storage

let wallet_main (param, s : wallet_entrypoints * wallet_storage) : return = begin
  match param with
    | Send amt -> 
      let u : unit = fail_if_paused(s)  in 
      let v : unit = assert_msg(amt <= Tezos.balance, "INSUFFICIENT_BALANCE") in 
      let admin : unit contract = resolve_address(s.admin) in 
      let empty_wallet_op : operation = Tezos.transaction () amt admin in 
      ([empty_wallet_op], s)
    | Default -> 
      let u : unit = fail_if_paused(s) in 
      (([] : operation list), s)
    | Admin a ->
      let u : unit = fail_if_not_admin(s) in
      let (ops, new_admin) = admin_main (a, s) in
      (ops, new_admin)
  end