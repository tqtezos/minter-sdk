#if !FA2_MAC_TOKEN
#define FA2_MAC_TOKEN

#include "../../../fa2/fa2_interface.mligo"
#include "../../../fa2/fa2_errors.mligo"
#include "../../../fa2/lib/fa2_operator_lib.mligo"

type royalty_transfer = {
  tez      : tez;
  transfer : transfer;
}

type fa2_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  | Royalty_transfer of royalty_transfer list
  (* | Token_metadata_registry of address contract *)

(* (owner,token_id) -> balance *)
type ledger = ((address * token_id), nat) big_map

(* token_id -> total_supply *)
type token_total_supply = (token_id, nat) big_map

type royalty = {
  scheme : (address, nat) map;
  total  : nat;
}

type rates = (token_id, royalty) big_map

#if !LIMITED_TOKEN_MANAGER

type multi_ft_token_storage = {
  ledger : ledger;
  operators : operator_storage;
  token_total_supply : token_total_supply;
  token_metadata : token_metadata_storage;
  rates : rates;
}

#else

type multi_ft_token_storage = {
  ledger : ledger;
  operators : operator_storage;
  token_total_supply : token_total_supply;
  token_metadata : token_metadata_storage;
  next_token_id : token_id;
  rates : rates;
}

#endif

(**
Ensures sum of (tez associated with each transfer) do not exceed Tezos.amount.
 *)
let verify_royalty_transfer_list (rts : royalty_transfer list) : unit =
  let amt : tez = Tezos.amount in
  let sum (acc, rt : tez * royalty_transfer) : tez = acc + rt.tez in
  let tez_sum = List.fold_left sum 0tez rts in
  if tez_sum <= amt then () : unit
  else (failwith "List of tez to associate w/ each transfer not valid!" : unit)

let get_balance_amt (key, ledger : (address * nat) * ledger) : nat =
  let bal_opt = Big_map.find_opt key ledger in
  match bal_opt with
  | None -> 0n
  | Some b -> b

let inc_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger) : ledger =
  let key = owner, token_id in
  let bal = get_balance_amt (key, ledger) in
  let updated_bal = bal + amt in
  if updated_bal = 0n
  then Big_map.remove key ledger
  else Big_map.update key (Some updated_bal) ledger

let dec_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger) : ledger =
  let key = owner, token_id in
  let bal = get_balance_amt (key, ledger) in
  match Michelson.is_nat (bal - amt) with
  | None -> (failwith fa2_insufficient_balance : ledger)
  | Some new_bal ->
    if new_bal = 0n
    then Big_map.remove key ledger
    else Big_map.update key (Some new_bal) ledger

(**
Update leger balances according to the specified transfers. Fails if any of the
permissions or constraints are violated.
@param txs transfers to be applied to the ledger
@param validate_op function that validates of the tokens from the particular owner can be transferred.
 *)
let transfer (txs, validate_op, storage
    : (transfer list) * operator_validator * multi_ft_token_storage)
    : ledger =
  let make_transfer = fun (l, tx : ledger * transfer) ->
    List.fold
      (fun (ll, dst : ledger * transfer_destination) ->
        if not Big_map.mem dst.token_id storage.token_metadata
        then (failwith fa2_token_undefined : ledger)
        else
          let u = validate_op (tx.from_, Tezos.sender, dst.token_id, storage.operators) in
          let lll = dec_balance (tx.from_, dst.token_id, dst.amount, ll) in
          inc_balance(dst.to_, dst.token_id, dst.amount, lll)
      ) tx.txs l
  in
  List.fold make_transfer txs storage.ledger

let royalty_transfer (rts, validate_op, storage
    : (royalty_transfer list) * operator_validator * multi_ft_token_storage)
    : operation list * multi_ft_token_storage =
  let oplist_concat = (fun (left, right : operation list * operation list) -> // want some way to concat op lists
    let (concat = fun (acc, op : operation list, operation) -> acc :: op) in
    List.fold_left concat left right) in
  let token_payments = (fun (tok_id, amt, current_owner: token_id, tez, address) -> // create a list of ops for the payments
    let scheme : (address, nat) map = (Big_map.find_opt tok_id storage.rates).scheme in
    let total : nat = (Big_map.find_opt tok_id storage.rates).total in
    let get_payment_op = (fun (acc, entry : op list * (address, nat)) -> // to itereate over the entries and add the op of each
      (if entry.0 = Tezos.self_address then
        let op = Tezos.transaction (() : unit) ((entry.1 * amt) / total) current_owner
      else let op = Tezos.trasnaction (() : unit) ((entry.1 * amt) / total) entry.0) in
      acc :: op
    ) in
  Map.fold get_payment_op scheme ([] : operation list)
  ) in
  verify_royalty_transfer_list (rts) in       // rts must be valid
  let make_transfer = fun (l, rt : ledger * royalty_transfer) ->
    List.fold
      (fun (ll, dst : ledger * transfer_destination) ->
        if not Big_map.mem dst.token_id storage.token_metadata
        then (failwith fa2_token_undefined : ledger)
        else
          let u = validate_op (rt.transfer.from_, Tezos.sender, dst.token_id, storage.operators) in
          let lll = dec_balance (rt.transfer.from_, dst.token_id, dst.amount, ll) in
          inc_balance(dst.to_, dst.token_id, dst.amount, lll) in

      ) rt.transfer.txs l
  in
  let make_payment = fun (ops, rt : operation list * royalty_transfer) ->
    List.fold
      (fun (opss, dst : operation list * transfer_destination) ->
          oplist_concat opss (token_payments (dst.token_id, rt.tez, rt.transfer.from_))
      ) rt.transfer.txs ops
  let new_storage : multi_ft_token_storage = { storage with ledger = List.fold make_transfer rts storage.ledger; } in
  let ops : operation list = List.fold make_payment rts ([] : operation list) in
  (ops, new_storage)

let get_balance (p, ledger, tokens
    : balance_of_param * ledger * token_metadata_storage) : operation =
  let to_balance = fun (r : balance_of_request) ->
    if not Big_map.mem r.token_id tokens
    then (failwith fa2_token_undefined : balance_of_response)
    else
      let key = r.owner, r.token_id in
      let bal = get_balance_amt (key, ledger) in
      let response : balance_of_response = { request = r; balance = bal; } in
      response
  in
  let responses = List.map to_balance p.requests in
  Tezos.transaction responses 0mutez p.callback


let fa2_main (param, storage : fa2_entry_points * multi_ft_token_storage)
    : (operation  list) * multi_ft_token_storage =
  match param with
  | Transfer txs ->
    (*
    will validate that a sender is either `from_` parameter of each transfer
    or a permitted operator for the owner `from_` address.
    *)
    let new_ledger = transfer (txs, default_operator_validator, storage) in
    let new_storage = { storage with ledger = new_ledger; }
    in ([] : operation list), new_storage

  | Royalty_transfer rts ->
    royalty_transfer (rts, default_operator_validator, storage)

  | Balance_of p ->
    let op = get_balance (p, storage.ledger, storage.token_metadata) in
    [op], storage

  | Update_operators updates ->
    let new_ops = fa2_update_operators (updates, storage.operators) in
    let new_storage = { storage with operators = new_ops; } in
    ([] : operation list), new_storage

#endif
