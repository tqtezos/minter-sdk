#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/non_pausable_simple_admin.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_simple.mligo"
#include "fa2_swap.mligo"

(* ======== Swaps allowlist component ======== *)

(* ==== Types ==== *)

let swap_check_allowlist(allowlist, swap_param : allowlist * swap_entrypoints) : unit =
  match swap_param with
    Start swap_offer -> begin
      [@inline]
      let check_asset(on_err : string)(assets : fa2_assets) : unit =
        match Big_map.find_opt assets.fa2_address allowlist with
        | Some u -> u
        | None -> failwith on_err
        in
      List.iter (check_asset "SWAP_OFFERED_FA2_NOT_ALLOWLISTED")
                swap_offer.assets_offered;
      List.iter (check_asset "SWAP_REQUESTED_FA2_NOT_ALLOWLISTED")
                swap_offer.assets_requested;
      unit
      end
  | Accept -> unit
  | Cancel -> unit

(* ======== Swaps allowlisted contract ======== *)

type storage =
  { swap : swap_storage
  ; admin : admin_storage
  ; allowlist : allowlist
  }

type entrypoints =
  | Swap of swap_entrypoints
  | Admin of admin_entrypoints
  | Update_allowed of allowlist

let allowlisted_swaps_main(param, storage : entrypoints * storage)
    : ((operation list) * storage) =
  let swap_storage = storage.swap in
  let allowlist = storage.allowlist in

  match param with
    Swap swap_param -> begin
      swap_check_allowlist(allowlist, swap_param);
      let (ops, swap_storage) = swaps_main(swap_param, swap_storage) in
      (ops, { storage with swap = swap_storage })
      end
  | Admin admin_param ->
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      ops, { storage with admin = admin_storage }
  | Update_allowed new_allowed -> begin
      fail_if_not_admin storage.admin;
      (([] : operation list), { storage with allowlist = new_allowed })
      end
