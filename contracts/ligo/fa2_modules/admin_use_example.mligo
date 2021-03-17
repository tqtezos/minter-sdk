(**
This is an example how to incorporate admin module into a contract
 *)

(* pick one of the admin implementations *)
(* #include "null_admin.mligo" *)
#include "simple_admin.mligo"
(* #include "multi_admin.mligo" *)


type storage = {
  number : nat;
  admin : admin_storage;
}

type entrypoints =
  | Increment of nat
  | Admin of admin_entrypoints

let main(p, s : entrypoints * storage) =
  match p with
  | Increment n ->
    let u = fail_if_paused s.admin in
    let new_s = { s with number = s.number + n; } in
    ([] : operation list), new_s

  | Admin a ->
    let u = fail_if_not_admin s.admin (None : string option) in
    let ops, new_admin = admin_main (a, s.admin) in
    ops, {s with admin = new_admin; }