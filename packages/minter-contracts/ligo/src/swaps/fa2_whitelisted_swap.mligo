#import "../../fa2/fa2_interface.mligo" "FA2"
#include "../../fa2_modules/admin/non_pausable_simple_admin.mligo"
#include "fa2_swap.mligo"

(* ======== Swaps whitelist component ======== *)

(* ==== Types ==== *)

type whitelist = (address, unit) big_map

let swap_check_whitelist(whitelist, swap_param : whitelist * swap_entrypoints) : unit =
  match swap_param with
    Start swap_offer -> begin
      [@inline]
      let check_asset(on_err : string)(assets : fa2_assets) : unit =
        match Big_map.find_opt assets.fa2_address whitelist with
        | Some u -> u
        | None -> failwith on_err
        in
      List.iter (check_asset "SWAP_OFFERED_FA2_NOT_WHITELISTED")
                swap_offer.assets_offered;
      List.iter (check_asset "SWAP_REQUESTED_FA2_NOT_WHITELISTED")
                swap_offer.assets_requested;
      unit
      end
  | Accept -> unit
  | Cancel -> unit

(* ======== Swaps whitelisted contract ======== *)

type storage =
  { swap : swap_storage
  ; admin : admin_storage
  ; whitelist : whitelist
  }

type entrypoints =
  | Swap of swap_entrypoints
  | Admin of admin_entrypoints
  | Update_allowed of (address, unit) big_map

let whitelisted_swaps_main(param, storage : entrypoints * storage)
    : ((operation list) * storage) =
  let swap_storage = storage.swap in
  let whitelist = storage.whitelist in

  match param with
    Swap swap_param -> begin
      swap_check_whitelist(whitelist, swap_param);
      let (ops, swap_storage) = swaps_main(swap_param, swap_storage) in
      (ops, { storage with swap = swap_storage })
      end
  | Admin admin_param ->
      let (ops, admin_storage) = admin_main(admin_param, storage.admin) in
      ops, { storage with admin = admin_storage }
  | Update_allowed new_allowed -> begin
      fail_if_not_admin storage.admin;
      (([] : operation list), { storage with whitelist = new_allowed })
      end
