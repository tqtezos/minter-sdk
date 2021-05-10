#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../common.mligo"

type sale_data =
[@layout:comb]
{
  sale_price: nat;
  sale_token : global_token_id;
  money_token : global_token_id;
  amount : nat;
}

type sale =
[@layout:comb]
{
  seller: address;
  sale_data: sale_data;
}

#if !FEE 

type storage = 
[@layout:comb]
{
    admin: pauseable_admin_storage;
    sales: (sale_id, sale) big_map;
    next_sale_id : sale_id;
}

#else 

type storage = 
[@layout:comb]
{
    admin: pauseable_admin_storage;
    sales: (sale_id, sale) big_map;
    next_sale_id : sale_id;
    fee : fee_data;
}

#endif

type market_entry_points =
  | Sell of sale_data
  | Buy of sale_id
  | Cancel of sale_id
  | Admin of pauseable_admin

let buy_token(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let sale : sale = match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE": sale)
    | Some s -> s
  in
  let seller = sale.seller in 
  let token_for_sale_address = sale.sale_data.sale_token.fa2_address in 
  let token_for_sale_token_id = sale.sale_data.sale_token.token_id in 
  let money_token_address = sale.sale_data.money_token.fa2_address in 
  let money_token_id = sale.sale_data.money_token.token_id in
  let sale_price = sale.sale_data.sale_price in
  let amount_ = sale.sale_data.amount in 

  let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, 1n , Tezos.self_address, Tezos.sender) in
#if !FEE 
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price, Tezos.sender, seller) in
  let oplist : operation list = [tx_price; tx_nft] in 
#else 
  let fee : nat = percent_of_bid_nat (storage.fee.fee_percent, sale_price) in
  let sale_price_minus_fee : nat =  (match (is_nat (sale_price - fee)) with 
    | Some adjusted_price -> adjusted_price 
    | None -> (failwith "FEE_TOO_HIGH" : nat)) in
  let tx_fee : operation = transfer_fa2(money_token_address, money_token_id, fee, Tezos.sender, storage.fee.fee_address) in
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price_minus_fee, Tezos.sender, seller) in
  let oplist : operation list = [tx_price; tx_nft; tx_fee] in
#endif
  let new_sales : (sale_id, sale) big_map = 
    if sale.sale_data.amount <= 1n 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with sale_data.amount = abs (amount_ - 1n)}) storage.sales in
  let new_s = {storage with sales = new_sales } in
  oplist, new_s

let deposit_for_sale(sale_data, storage: sale_data * storage) : (operation list * storage) =
  let token_for_sale_address = sale_data.sale_token.fa2_address in 
  let token_for_sale_token_id = sale_data.sale_token.token_id in 
  let amount_ = sale_data.amount in 
  let transfer_op =
    transfer_fa2 (token_for_sale_address, token_for_sale_token_id, amount_, Tezos.sender, Tezos.self_address) in
  let sale = { seller = Tezos.sender; sale_data = sale_data; } in
  let sale_id = storage.next_sale_id in
  let new_s = { storage with sales = Big_map.add sale_id sale storage.sales;
                next_sale_id = sale_id + 1n} in
  ([transfer_op]), new_s

let cancel_sale(sale_id, storage: sale_id * storage) : (operation list * storage) = 
  match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE" : (operation list * storage))
    | Some sale ->  let token_for_sale_address = sale.sale_data.sale_token.fa2_address in
                    let token_for_sale_token_id = sale.sale_data.sale_token.token_id in
                    let amount_ = sale.sale_data.amount in 
                    let seller = sale.seller in
                    let is_seller = Tezos.sender = seller in
                    let v : unit = if is_seller then ()
                      else fail_if_not_admin_ext (storage.admin, "OR A SELLER") in
                    let tx_nfts_back_op = transfer_fa2(token_for_sale_address, token_for_sale_token_id, amount_, Tezos.self_address, seller) in
                    ([tx_nfts_back_op]), { storage with sales = Big_map.remove sale_id storage.sales }

let fixed_price_sale_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sale_data ->
     let u : unit = fail_if_paused(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
     let w : unit = fail_if_not_admin(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
     deposit_for_sale(sale_data, storage)
  | Buy sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
     buy_token(sale_id, storage)
  | Cancel sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
     cancel_sale(sale_id,storage)
  | Admin a ->
    let ops, admin = pauseable_admin(a, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage
  