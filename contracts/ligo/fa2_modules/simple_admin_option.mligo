
(*
  One of the possible implementations of admin API for FA2 contract.
  The admin API can change an admin address using two step confirmation pattern.
  Only current admin can initiate those operations.

  Other entry points may guard their access using helper functions
  `fail_if_not_admin`.
*)

#if !SIMPLE_ADMIN
#define SIMPLE_ADMIN

(* `simple_admin` entry points *)
type simple_admin =
  | Set_admin of address
  | Confirm_admin of unit


type simple_admin_storage_record = {
  admin : address;
  pending_admin : address option;
}

type simple_admin_storage = simple_admin_storage_record option

let confirm_new_admin (storage : simple_admin_storage) : simple_admin_storage =
  match storage with
    | Some s ->
        ( match s.pending_admin with
          | None -> (failwith "NO_PENDING_ADMIN" : simple_admin_storage)
          | Some pending ->
            if Tezos.sender = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = Tezos.sender;
            } : simple_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : simple_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : simple_admin_storage)

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin (storage : simple_admin_storage) (extra_msg : string option) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then match extra_msg with
             | None -> failwith "NOT_AN_ADMIN"
             | Some msg -> failwith ("NOT_AN_ADMIN" ^  " "  ^ msg)
        else unit
    | None -> unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * simple_admin_storage) : simple_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : simple_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : simple_admin_storage)

let simple_admin (param, storage : simple_admin *simple_admin_storage)
    : (operation list) * (simple_admin_storage) =
  match param with
  | Set_admin new_admin ->
      let new_s = set_admin (new_admin, storage) in
      (([] : operation list), new_s)

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)


#endif
