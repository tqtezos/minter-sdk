(*
  One of the possible implementations of admin API for FA2 contract.
  The admin API can change an admin address using two step confirmation pattern and
  pause/unpause the contract. Only current admin can initiate those operations.

  Other entry points may guard their access using helper functions
  `fail_if_not_admin` and `fail_if_paused`.
*)

#if !PAUSEABLE_ADMIN
#define PAUSEABLE_ADMIN

(* `pauseable_admin` entry points *)
type pauseable_admin =
  | Set_admin of address
  | Confirm_admin of unit
  | Pause of bool


type pauseable_admin_storage_record = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}

type pauseable_admin_storage = pauseable_admin_storage_record option

let confirm_new_admin (storage : pauseable_admin_storage) : pauseable_admin_storage =
  match storage with
    | Some s ->
        ( match s.pending_admin with
          | None -> (failwith "NO_PENDING_ADMIN" : pauseable_admin_storage)
          | Some pending ->
            if Tezos.sender = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = Tezos.sender;
            } : pauseable_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : pauseable_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
        else unit
    | None -> unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : pauseable_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

(*Only callable by admin*)
let pause (paused, storage: bool * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with paused = paused; } : pauseable_admin_storage_record ))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_paused (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if a.paused
        then failwith "PAUSED"
        else unit
    | None -> unit

let pauseable_admin (param, storage : pauseable_admin *pauseable_admin_storage)
    : (operation list) * (pauseable_admin_storage) =
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
