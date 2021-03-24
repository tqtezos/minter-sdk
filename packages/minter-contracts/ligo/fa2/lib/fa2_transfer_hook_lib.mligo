(**
 Helper types and functions to implement transfer hook contract.
 Each transfer hook contract maintains a registry of known FA2 contracts and
 validates that it is invoked from registered FA2 contracts.

 The implementation assumes that the transfer hook entrypoint is labeled as
 `%tokens_transferred_hook`.
 *)

#if !FA2_HOOK_LIB
#define FA2_HOOK_LIB

#include "../fa2_hook.mligo"

let get_hook_entrypoint (hook_contract : address) (u : unit)
    : transfer_descriptor_param contract option =
  let hook_entry : transfer_descriptor_param contract =
    Tezos.get_entrypoint_opt "%tokens_transferred_hook" hook_contract in
  hook_entry


let create_register_hook_op
    (fa2, descriptor : (fa2_with_hook_entry_points contract) * permissions_descriptor) : operation =
  let mhook_fn = get_hook_entrypoint Current.self_address in
  let hook_fn = match mhook_fn with
    | None -> (failwith "CANNOT_GET_HOOK_FUNCTION" : transfer_descriptor_param contract)
    | Some hook_fn -> hook_fn
  in let p : set_hook_param = {
    hook = hook_fn;
    permissions_descriptor = descriptor;
  } in
  Tezos.transaction (Set_transfer_hook p) 0mutez fa2


type fa2_registry = address set

let register_with_fa2 (fa2, descriptor, registry :
    (fa2_with_hook_entry_points contract) * permissions_descriptor * fa2_registry)
    : operation * fa2_registry =
  let op = create_register_hook_op (fa2, descriptor) in
  let fa2_address = Current.address fa2 in
  let new_registry = Set.add fa2_address registry in
  op, new_registry

let validate_hook_call (fa2, registry: address * fa2_registry) : unit =
  if Set.mem fa2 registry
  then unit
  else failwith "UNKNOWN_FA2_CALL"

#endif
