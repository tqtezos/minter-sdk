type ledger = (address, nat) big_map

type storage = {
  ledger : ledger;
  admin : admin_storage;
  minter_admin : minter_admin_storage;
}

type tx = {
  to_: address;
  amount: nat;
}

type entrypoints =
  | Transfer of tx
  | Mint of nat
  | Admin of admin_entrypoints
  | MinterAdmin of minter_admin_entrypoints


[@inline]
let fail_if_not_minter(storage : storage) : unit =
  if is_admin(storage.admin)
  then unit (* admin can always mint *)
  else if is_minter(storage.minter_admin)
  then unit (* minter can mint *)
  else failwith "NOT_A_MINTER"


let mint (ledger, amt : ledger * nat) : ledger =
  let old_bal = match Big_map.find_opt Tezos.sender ledger with
  | None -> 0n
  | Some bal -> bal
  in
  Big_map.update Tezos.sender (Some (old_bal + amt)) ledger

let transfer (storage, tx : storage * tx) : storage =
  let src_bal = match Big_map.find_opt Tezos.sender storage.ledger with
  | None -> 0n
  | Some bal -> bal 
  in
  let l1 = match Michelson.is_nat (src_bal - tx.amount) with
  |None -> (failwith "NO_FUNDS" : ledger)
  |Some new_bal -> 
      Big_map.update Tezos.sender (Some new_bal) storage.ledger
  in

  let dst_bal = match Big_map.find_opt tx.to_ storage.ledger with
  | None -> tx.amount
  | Some bal -> bal + tx.amount
  in
  let l2 = Big_map.update tx.to_ (Some dst_bal) l1 in
  {storage with ledger = l2; }

let main(p, s : entrypoints * storage) : (operation list) * storage =
  match p with
  | Transfer t ->
    let u = fail_if_paused s.admin in
    let new_s = transfer (s, t) in
    ([] : operation list), new_s


  | Mint amt -> 
    let u1 = fail_if_paused s.admin in
    let u2 = fail_if_not_minter s in
    let new_ledger = mint (s.ledger, amt) in
    ([] : operation list), {s with ledger = new_ledger; }

  | Admin a ->
    let u = fail_if_not_admin s.admin in
    let ops, new_admin = admin_main (a, s.admin) in
    ops, {s with admin = new_admin; }

  | MinterAdmin a -> 
    let u = fail_if_not_admin_ext (s.admin, "ONLY_ADMIN_CAN_CHANGE_MINTER") in
    let ops, new_admin = minter_admin_main (a, s.minter_admin) in
    ops, {s with minter_admin = new_admin; }

