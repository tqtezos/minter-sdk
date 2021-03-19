(* pick which admin and minter module implementations you want to use here *)

(* #include "../admin/no_admin.mligo" *)
#include "../admin/simple_admin.mligo"
(* #include "../admin/non_pausable_simple_admin.mligo" *)
(* #include "../admin/multi_admin.mligo" *)

(* #include "../minter_admin/not_minter_admin.mligo" *)
(* #include "../minter_admin/null_minter_admin.mligo" *)
#include "../minter_admin/multi_minter_admin.mligo"

#include "minter_use_example.mligo"

(* uses main function from  minter_use_example.mligo *)