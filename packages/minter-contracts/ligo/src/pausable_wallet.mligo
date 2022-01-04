(*
  A basic wallet contract to which funds can be sent and stored, integrated with the simple-admin module to 
  provide locking capability to the default entrypoint. Funds can be sent to the %defualt entrypoint of the contract 
  if and only if the contract is unlocked. The contract admin can send a specified amount of the wallet's balance to themselves 
  by calling the %send entrypoint of the contract along with the amount in tez they would like to send themselves. 
 *)

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
      let u : unit = fail_if_not_admin(s) in (*Only admin can send funds*)
      let admin : unit contract = resolve_address(s.admin) in 
      let send_op : operation = Tezos.transaction () amt admin in 
      ([send_op], s)
    | Default -> 
      let u : unit = fail_if_paused(s) in (*Wallet cannot receive funds from extrernal users when the contract is paused*)
      (([] : operation list), s)
    | Admin a ->
      let (ops, new_admin) = admin_main (a, s) in
      (ops, new_admin)
  end