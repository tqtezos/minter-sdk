#include "english_auction_tez_fixed_fee.mligo"
#include "../../fa2_modules/allowlist.mligo"

type allowlisted_entry_points
  = Call_auction of auction_entrypoints
  | Update_allowed of allowlist

type allowlisted_storage =
  { auction_storage : storage
  ; allowlist : allowlist
  }

let check_allowed_param (param, allowlist : auction_entrypoints * allowlist) : unit =
  match param with
  | Configure p ->
      List.iter
        (fun (token : tokens) ->
          check_address_allowed (token.fa2_address, allowlist, "ASSET_ADDRESS_NOT_ALLOWED")
        )
        p.asset
  | AdminAndInteract p -> unit

let english_auction_tez_allowlisted_main (param, storage : allowlisted_entry_points * allowlisted_storage)
    : operation list * allowlisted_storage = match param with
  | Call_auction auction_param ->
      let u : unit = check_allowed_param (auction_param, storage.allowlist) in
      let ops, auction_storage = english_auction_tez_main(auction_param, storage.auction_storage) in
      ops, { storage with auction_storage = auction_storage }
  | Update_allowed new_allowlist -> begin
      fail_if_not_admin(storage.auction_storage.pauseable_admin);
      ( ([] : operation list)
      , { storage with allowlist = new_allowlist }
      )
      end
