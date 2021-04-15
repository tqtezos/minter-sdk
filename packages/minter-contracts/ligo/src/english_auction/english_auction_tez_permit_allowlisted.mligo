#include "english_auction_tez_permit.mligo"
#include "../../fa2_modules/allowlist.mligo"

type allowlisted_entry_points
  = Call_auction of permit_auction_entrypoints
  | Update_allowed of allowlist

type allowlisted_storage =
  { permit_auction_storage : permit_storage
  ; allowlist : allowlist
  }

let check_allowed_param (param, allowlist : permit_auction_entrypoints * allowlist) : unit =
  match param with
  | Permit_configure p ->
      List.iter
        (fun (cp : permit_config_param) ->
          List.iter
            (fun (token : tokens) ->
              check_address_allowed (token.fa2_address, allowlist, "ASSET_ADDRESS_NOT_ALLOWED")
            )
            cp.config.asset
        )
        p
  | Auction p -> unit

let english_auction_tez_permit_allowlisted_main (param, storage : allowlisted_entry_points * allowlisted_storage)
    : operation list * allowlisted_storage = match param with
  | Call_auction auction_param ->
      let u : unit = check_allowed_param (auction_param, storage.allowlist) in
      let ops, permit_auction_storage = english_auction_tez_permit_main(auction_param, storage.permit_auction_storage) in
      ops, { storage with permit_auction_storage = permit_auction_storage }
  | Update_allowed new_allowlist -> begin
      fail_if_not_admin(storage.permit_auction_storage.auction_storage.pauseable_admin);
      ( ([] : operation list)
      , { storage with allowlist = new_allowlist }
      )
      end
