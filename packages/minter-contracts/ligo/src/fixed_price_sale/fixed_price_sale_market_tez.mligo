#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"

type sale_data_tez =
[@layout:comb]
{
  sale_token: global_token_id;
  price: tez;
  amount: nat;
}

#if !PERMIT_MARKET 
type sale_tez =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data_tez;
}
#else 
type sale_tez =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data_tez;
  pending_purchases : address set;
}
#endif

#if !FEE

type storage =
[@layout:comb]
{
  sales: (sale_id, sale_tez) big_map;
  admin: pauseable_admin_storage;
  next_sale_id : sale_id;
  allowlist : allowlist;
}

#else

type storage =
[@layout:comb]
{
  sales: (sale_id, sale_tez) big_map;
  admin: pauseable_admin_storage;
  next_sale_id : sale_id;
  allowlist : allowlist;
  fee : fee_data;
}

#endif

type market_entry_points = 
  | Buy of sale_id 
  | Sell of sale_data_tez
  | Cancel of sale_id
  | Admin of pauseable_admin
  | Update_allowed of allowlist_entrypoints

let get_sale(sale_id, storage: sale_id * storage) : sale_tez = 
   (match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE": sale_tez)
    | Some s -> s)

let buy_token(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let sale : sale_tez = get_sale(sale_id, storage) in
  let sale_price : tez = sale.sale_data.price in
  let sale_token_address : address = sale.sale_data.sale_token.fa2_address in
  let sale_token_id : nat = sale.sale_data.sale_token.token_id in
  let amount_ : nat = sale.sale_data.amount in
  let seller : address = sale.seller in
  let amountError : unit =
    if Tezos.amount <> sale_price
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", sale_price, Tezos.amount) : unit)
    else () in
  let tx_nft = transfer_fa2(sale_token_address, sale_token_id, 1n, Tezos.self_address, Tezos.sender) in
  let oplist : operation list = [tx_nft] in
#if !FEE
  let oplist =
    (if sale_price <> 0mutez
     then
       let tx_price = transfer_tez(sale_price, seller) in
       tx_price :: oplist
     else oplist) in
#else
  let fee : tez = percent_of_price_tez (storage.fee.fee_percent, sale_price) in
  let u : unit = assert_msg(sale_price >= fee, "FEE_TO_HIGH") in
  let sale_price_minus_fee : tez = sale_price - fee in
  let oplist =
    (if fee <> 0mutez
     then
       let tx_fee : operation = transfer_tez(fee, storage.fee.fee_address) in
       tx_fee :: oplist
     else oplist) in
  let oplist =
    (if sale_price_minus_fee <> 0mutez
     then
       let tx_price = transfer_tez(sale_price_minus_fee, seller) in
       tx_price :: oplist
     else oplist) in
#endif
  let new_sales : (sale_id, sale_tez) big_map =
    if sale.sale_data.amount <= 1n
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with sale_data.amount = abs (amount_ - 1n)}) storage.sales in
  let new_s = { storage with sales = new_sales } in
  oplist, new_s

let tez_stuck_guard(entrypoint: string) : string = "DON'T TRANSFER TEZ TO THIS ENTRYPOINT (" ^ entrypoint ^ ")"

let check_allowlisted (data, allowlist : sale_data_tez * allowlist) : unit = begin
  check_single_token_allowed (data.sale_token.fa2_address, data.sale_token.token_id, allowlist, "SALE_TOKEN_NOT_ALLOWED");
  unit end

let deposit_for_sale(sale_data, storage: sale_data_tez * storage) : (operation list * storage) =
    let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "SELL") else () in
    let u : unit = check_allowlisted(sale_data, storage.allowlist) in
    let sale_price : tez = sale_data.price in
    let sale_token_address : address = sale_data.sale_token.fa2_address in
    let sale_token_id : nat = sale_data.sale_token.token_id in
    let amount_ : nat = sale_data.amount in
    let transfer_op =
      transfer_fa2 (sale_token_address, sale_token_id, amount_, Tezos.sender, Tezos.self_address) in
#if !PERMIT_MARKET 
    let sale = { seller = Tezos.sender; sale_data = sale_data; } in
#else 
    let sale = { seller = Tezos.sender; sale_data = sale_data; pending_purchases = (Set.empty : address set)} in
#endif
    let sale_id : sale_id = storage.next_sale_id in 
    let new_s = { storage with sales = Big_map.add sale_id sale storage.sales; 
      next_sale_id =  sale_id + 1n; } in
    [transfer_op], new_s

let cancel_sale(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "CANCEL") else () in
  let sale : sale_tez = get_sale(sale_id, storage) in
  let sale_token_address = sale.sale_data.sale_token.fa2_address in 
  let sale_token_id = sale.sale_data.sale_token.token_id in 
  let amount_ = sale.sale_data.amount in 
  let seller = sale.seller in 
  let is_seller = Tezos.sender = seller in
  let v : unit = if is_seller then ()
    else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
#if PERMIT_MARKET
    let sale : sale_tez = get_sale(sale_id, storage) in 
    let u : unit = assert_msg(Set.size sale.pending_purchases = 0n, "PENDING_PURCHASES_PRESENT") in
#endif
  let tx_nft_back_op = transfer_fa2(sale_token_address, sale_token_id, amount_, Tezos.self_address, seller) in
  [tx_nft_back_op], {storage with sales = Big_map.remove sale_id storage.sales }

let update_allowed(allowlist_param, storage : allowlist_entrypoints * storage) : operation list * storage =
#if !ALLOWLIST_ENABLED
    [%Michelson ({| { NEVER } |} : never -> operation list * storage)] allowlist_param
#else
    let u : unit = fail_if_not_admin(storage.admin) in
    let allowlist_storage = update_allowed (allowlist_param, storage.allowlist) in
    ([] : operation list), { storage with allowlist = allowlist_storage }
#endif

let fixed_price_sale_tez_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sale_data ->
     let u : unit = fail_if_paused(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif

#if !CANCEL_ONLY_ADMIN
     let w : unit = fail_if_not_admin(storage.admin) in
#endif 
     deposit_for_sale(sale_data, storage)
  | Cancel sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
     cancel_sale(sale_id,storage)
  | Admin a ->
     let ops, admin = pauseable_admin(a, storage.admin) in
     let new_storage = { storage with admin = admin; } in
     ops, new_storage
  | Update_allowed a ->
    update_allowed(a, storage)
  | Buy sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
     buy_token(sale_id, storage)

#if !FEE

let sample_storage : storage =
  {
    admin = Some ({
        admin =  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
        pending_admin = (None : address option);
        paused = false;});
    sales = (Big_map.empty : (sale_id, sale_tez) big_map);
    next_sale_id = 0n;
    allowlist = init_allowlist;
  }

#else

let sample_storage : storage =
  {
    admin = Some ( {
        admin =  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
        pending_admin = (None : address option);
        paused = false;
              });
    sales = (Big_map.empty : (sale_id, sale_tez) big_map);
    next_sale_id = 0n;
    allowlist = init_allowlist;
    fee = {
      fee_address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
      fee_percent = 15n;
    }
  }
#endif

(*VIEWS*)
let rec activeSalesHelper (active_sales, sale_id, s : (sale_tez list) * sale_id * storage)
  : (sale_tez list) =
  (if sale_id >= s.next_sale_id
  then active_sales
  else ( match (Big_map.find_opt sale_id s.sales) with
    | Some sale -> activeSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveSales (initial_sale_id , s : sale_id * storage) : (sale_tez list) =
  (activeSalesHelper (([] : sale_tez list), initial_sale_id,  s))
