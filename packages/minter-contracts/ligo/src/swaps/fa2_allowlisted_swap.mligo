#include "../../fa2_modules/admin/non_pausable_simple_admin.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_simple.mligo"
#include "fa2_swap.mligo"

(* ==== Types ==== *)
type storage =
  { swap : swap_storage
  ; admin : admin_storage
  ; allowlist : allowlist
  }

type entrypoints =
  | Swap of swap_entrypoints
  | Admin of admin_entrypoints
  | Update_allowed of allowlist
#if CHANGE_BURN_ADDRESS
  | Change_burn_address of address
#endif

type allowlist_return = ((operation list) * storage)

(* ======== Swaps allowlist component ======== *)

let swap_check_allowlist(allowlist, storage, swap_param : allowlist * storage * swap_entrypoints) : unit =
  match swap_param with
    Start swap_offers -> begin
      fail_if_not_admin(storage.admin);
      [@inline]
      let check_asset(on_err : string)(assets : fa2_assets) : unit =
        match Big_map.find_opt assets.fa2_address allowlist with
        | Some u -> u
        | None -> failwith on_err
        in
      List.iter (check_asset "SWAP_OFFERED_FA2_NOT_ALLOWLISTED")
                swap_offers.swap_offer.assets_offered;
      List.iter (check_asset "SWAP_REQUESTED_FA2_NOT_ALLOWLISTED")
#if !XTZ_FEE
                swap_offers.swap_offer.assets_requested;
#else
                swap_offers.swap_offer.assets_requested.0;
#endif
      unit
      end
  | Accept a -> unit
  | Cancel a -> unit

(* ======== Swaps allowlisted contract ======== *)

let allowlisted_swaps_main(param, storage : entrypoints * storage)
    : allowlist_return =
  let swap_storage = storage.swap in
  let allowlist = storage.allowlist in

  match param with
    Swap swap_param -> begin
      swap_check_allowlist(allowlist, storage, swap_param);
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
#if CHANGE_BURN_ADDRESS
  | Change_burn_address new_burn_address -> begin 
      fail_if_not_admin(storage.admin);
      (([] : operation list), { storage with swap  = { storage.swap with burn_address = new_burn_address}})
    end
#endif
