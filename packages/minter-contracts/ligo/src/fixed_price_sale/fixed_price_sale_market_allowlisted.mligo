#include "fixed_price_sale_market.mligo"
#include "../../fa2_modules/allowlist.mligo"

type allowlisted_entry_points
  = Call_market of market_entry_points
  | Update_allowed of allowlist

type allowlisted_storage =
  { market_storage : storage
  ; allowlist : allowlist
  }

let check_allowed_param (param, allowlist : market_entry_points * allowlist) : unit =
  match param with
  | ManageSale p -> ( match p with 
      | Sell p -> begin
          check_address_allowed (p.sale_token.fa2_address, allowlist, "SALE_ADDRESS_NOT_ALLOWED");
          unit
          end
      | Cancel p -> unit
      | Admin p -> unit )
  | Buy p -> unit

let fixed_price_sale_allowlisted_main (param, storage : allowlisted_entry_points * allowlisted_storage)
    : operation list * allowlisted_storage = match param with
  | Call_market market_param ->
      let u : unit = check_allowed_param (market_param, storage.allowlist) in
      let ops, market_storage = fixed_price_sale_main(market_param, storage.market_storage) in
      ops, { storage with market_storage = market_storage }
  | Update_allowed new_allowlist -> begin
      fail_if_not_admin(storage.market_storage.admin);
      ( ([] : operation list)
      , { storage with allowlist = new_allowlist }
      )
      end
