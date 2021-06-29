#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../../fa2_modules/fa2_allowlist/allowlist_base.mligo"
#include "../common.mligo"

#if !PER_SALE_FEE
type sale_data =
[@layout:comb]
{
  price: nat;
  sale_token : global_token_id;
  money_token : global_token_id;
  amount : nat;
}
#else 
type sale_data =
[@layout:comb]
{
  price: nat;
  sale_token : global_token_id;
  money_token : global_token_id;
  amount : nat;
  fee : fee_data;
}
#endif

#if !OFFCHAIN_MARKET
type sale =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data;
}
#else 
type sale =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data;
  pending_purchases : address set;
}
#endif

#if !FEE

type storage =
[@layout:comb]
{
    sales: (sale_id, sale) big_map;
    admin: pauseable_admin_storage;
    next_sale_id : sale_id;
    allowlist: allowlist;
}

#else

type storage =
[@layout:comb]
{
    sales: (sale_id, sale) big_map;
    admin: pauseable_admin_storage;
    next_sale_id : sale_id;
    allowlist: allowlist;
    fee : fee_data;
}

#endif

type market_entry_points = 
  | Buy of sale_id 
  | Sell of sale_data
  | Cancel of sale_id
  | Admin of pauseable_admin
  | Update_allowed of allowlist_entrypoints

let get_sale(sale_id, storage: sale_id * storage) : sale = 
   (match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE": sale)
    | Some s -> s)

let tx_price_and_fee (money_token_id, money_token_address, fee, fee_recipient_address, sale_price, seller, oplist :
   nat * address * nat * address * nat * address * operation list) = 
  let sale_price_minus_fee : nat =  (match (is_nat (sale_price - fee)) with
    | Some adjusted_price -> adjusted_price
    | None -> (failwith "FEE_TOO_HIGH" : nat)) in
  let tx_fee : operation = transfer_fa2(money_token_address, money_token_id, fee, Tezos.sender, fee_recipient_address) in
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price_minus_fee, Tezos.sender, seller) in
  (tx_price :: tx_fee :: oplist)

let buy_token(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let sale : sale = get_sale(sale_id, storage) in 
  let seller = sale.seller in 
  let token_for_sale_address = sale.sale_data.sale_token.fa2_address in 
  let token_for_sale_token_id = sale.sale_data.sale_token.token_id in 
  let money_token_address = sale.sale_data.money_token.fa2_address in 
  let money_token_id = sale.sale_data.money_token.token_id in
  let sale_price = sale.sale_data.price in
  let amount_ = sale.sale_data.amount in

  let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, 1n , Tezos.self_address, Tezos.sender) in
  let oplist : operation list = [tx_nft] in 
#if FEE
  let fee : nat = percent_of_price_nat (storage.fee.fee_percent, sale_price) in
  let fee_recipient_address : address = storage.fee.fee_address in 
  let oplist = tx_price_and_fee (money_token_id, money_token_address, fee, fee_recipient_address, sale_price, seller, oplist) in 
#elif PER_SALE_FEE
  let fee : nat = percent_of_price_nat (sale.sale_data.fee.fee_percent, sale_price) in
  let fee_recipient_address : address = sale.sale_data.fee.fee_address in 
  let oplist = tx_price_and_fee (money_token_id, money_token_address, fee, fee_recipient_address, sale_price, seller, oplist) in
#else
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price, Tezos.sender, seller) in
  let oplist : operation list = [tx_price; tx_nft] in
#endif

  let new_sales : (sale_id, sale) big_map =
    if sale.sale_data.amount <= 1n
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with sale_data.amount = abs (amount_ - 1n)}) storage.sales in
  let new_s = {storage with sales = new_sales } in
  oplist, new_s

let check_allowlisted (data, allowlist : sale_data * allowlist) : unit = begin
  check_single_token_allowed (data.sale_token.fa2_address, data.sale_token.token_id, allowlist, "SALE_TOKEN_NOT_ALLOWED");
  unit end

let deposit_for_sale(sale_data, storage: sale_data * storage) : (operation list * storage) =
  let u : unit = check_allowlisted(sale_data, storage.allowlist) in
  let token_for_sale_address = sale_data.sale_token.fa2_address in
  let token_for_sale_token_id = sale_data.sale_token.token_id in
  let amount_ = sale_data.amount in
  let transfer_op =
    transfer_fa2 (token_for_sale_address, token_for_sale_token_id, amount_, Tezos.sender, Tezos.self_address) in
#if !OFFCHAIN_MARKET 
  let sale = { seller = Tezos.sender; sale_data = sale_data; } in
#else 
  let sale = { seller = Tezos.sender; sale_data = sale_data; pending_purchases = (Set.empty : address set)} in
#endif
  let sale_id = storage.next_sale_id in
  let new_s = { storage with sales = Big_map.add sale_id sale storage.sales;
                next_sale_id = sale_id + 1n} in
  ([transfer_op]), new_s

let cancel_sale(sale_id, storage: sale_id * storage) : (operation list * storage) = 
    let sale : sale = get_sale(sale_id, storage) in 
    let token_for_sale_address = sale.sale_data.sale_token.fa2_address in
    let token_for_sale_token_id = sale.sale_data.sale_token.token_id in
    let amount_ = sale.sale_data.amount in 
    let seller = sale.seller in
    let is_seller = Tezos.sender = seller in
    let v : unit = if is_seller then ()
      else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
#if OFFCHAIN_MARKET
    let u : unit = assert_msg(Set.size sale.pending_purchases = 0n, "PENDING_PURCHASES_PRESENT") in
#endif
    let tx_nfts_back_op = transfer_fa2(token_for_sale_address, token_for_sale_token_id, amount_, Tezos.self_address, seller) in
    ([tx_nfts_back_op]), { storage with sales = Big_map.remove sale_id storage.sales }

let update_allowed(allowlist_param, storage : allowlist_entrypoints * storage) : operation list * storage =
#if !ALLOWLIST_ENABLED
    [%Michelson ({| { NEVER } |} : never -> operation list * storage)] allowlist_param
#else
    let u : unit = fail_if_not_admin(storage.admin) in
    let allowlist_storage = update_allowed (allowlist_param, storage.allowlist) in
    ([] : operation list), { storage with allowlist = allowlist_storage }
#endif

let fixed_price_sale_main (p, storage : market_entry_points * storage) : operation list * storage = 
  let u : unit = tez_stuck_guard("ANY_ENTRYPOINT") in 
  match p with
  | Buy sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
     buy_token(sale_id, storage)
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

(*VIEWS*)
let rec activeSalesHelper (active_sales, sale_id, s : (sale list) * sale_id * storage)
  : (sale list) =
  (if sale_id >= s.next_sale_id
  then active_sales
  else ( match (Big_map.find_opt sale_id s.sales) with
    | Some sale -> activeSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveSales (initial_sale_id , s : sale_id * storage) : (sale list) =
  (activeSalesHelper (([] : sale list), initial_sale_id,  s))
