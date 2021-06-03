#if !PERMIT_MARKET
#define PERMIT_MARKET

#include "fixed_price_sale_market.mligo"

type permit_storage = 
    [@layout:comb]
  {
    market_storage : storage;
    counter : nat;
  }

type permit_return = (operation list) * permit_storage

type offline_market_entry_points =
  | BaseSale of market_entry_points_without_buy
  | Permit_buy of permit_buy_param list 
  | Confirm_purchases of pending_purchases
  | Revoke_purchases of pending_purchases

let execute_pending_purchase (acc, pending_purchase: permit_return * pending_purchase ) : permit_return =
  let (oplist, permit_storage) = acc in 
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        buy_data = buy_data; } = pending_purchase in  
  let sale : sale = get_sale(sale_id, storage) in
  let { seller = seller;
        pending_purchases = pending_purchases;
        sale_data = {
            money_token = {
                token_id = money_token_id;
                fa2_address = money_token_address;
            };
            sale_token = {
                token_id = token_for_sale_token_id;
                fa2_address = token_for_sale_address;
            };
            sale_price = sale_price;
            amount = amount_;
        }

  } = sale in 
  
  let u : unit = assert_msg(Set.mem buy_data pending_purchases, "PURCHASE_NOT_FOUND") in 
  let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, 1n , Tezos.self_address, buy_data.purchaser) in
#if !FEE 
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price, Tezos.self_address, seller) in
  let oplist : operation list = tx_price :: tx_nft :: oplist in 
#else 
  let fee : nat = percent_of_price_nat (storage.fee.fee_percent, sale_price) in
  let sale_price_minus_fee : nat =  (match (is_nat (sale_price - fee)) with 
    | Some adjusted_price -> adjusted_price 
    | None -> (failwith "FEE_TOO_HIGH" : nat)) in
  let tx_fee : operation = transfer_fa2(money_token_address, money_token_id, fee, Tezos.self_address, storage.fee.fee_address) in
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price_minus_fee, Tezos.self_address, seller) in
  let oplist : operation list = tx_price :: tx_nft :: tx_fee :: oplist in
#endif
  let new_sales : (sale_id, sale) big_map = 
    if amount_ <= 0n && (Set.size pending_purchases <= 1n) 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with  
      pending_purchases = Set.remove buy_data pending_purchases}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  (oplist, new_s)

let confirm_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold execute_pending_purchase pending_purchases acc)

let revoke_pending_purchase (acc, pending_purchase: permit_return * pending_purchase ) : permit_return =
  let (oplist, permit_storage) = acc in 
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        buy_data = buy_data; } = pending_purchase in  
  let sale : sale = get_sale(sale_id, storage) in
  let { seller = seller;
        pending_purchases = pending_purchases;
        sale_data = {
            money_token = {
                token_id = money_token_id;
                fa2_address = money_token_address;
            };
            sale_token = {
                token_id = token_for_sale_token_id;
                fa2_address = token_for_sale_address;
            };
            sale_price = sale_price;
            amount = amount_;
        }

  } = sale in 
  let u : unit = assert_msg(Set.mem buy_data pending_purchases, "PURCHASE_NOT_FOUND") in 
  let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price, Tezos.self_address, buy_data.payment_relayer) in
  let oplist : operation list = tx_price :: oplist in 

  let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some {sale with sale_data = {sale.sale_data with amount = amount_ + 1n};
                                              pending_purchases = Set.remove buy_data pending_purchases}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  (oplist, new_s)

let revoke_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold revoke_pending_purchase pending_purchases acc)

let buy_token_pending_confirmation (sale_id, buy_data, is_permited, storage: sale_id * buy_data * bool * storage) : (operation option * storage) = begin 
    let sale : sale = get_sale(sale_id, storage) in
    let { seller = _;
          pending_purchases = pending_purchases;
          sale_data = {
              money_token = {
                  token_id = money_token_id;
                  fa2_address = money_token_address;
              };
              sale_token = _;
              sale_price = sale_price;
              amount = amount_;
          }
    } = sale in 
    assert_msg(amount_ >= 1n, "NO_SALE");
    (*Payment only necessary if buy order is not permited*)
    let maybe_tx_price : operation option = 
      if is_permited 
      then (None : operation option)
      else let tx_price = transfer_fa2(money_token_address, money_token_id, sale_price, buy_data.payment_relayer, Tezos.self_address) in
           (Some tx_price : operation option) in 
           
    let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some {sale with pending_purchases = (Set.add buy_data pending_purchases); 
                                   sale_data = {sale.sale_data with amount = abs (amount_ - 1n)}}) storage.sales in
    let new_s = {storage with sales = new_sales } in
    (maybe_tx_price, new_s)
  end 

let buy_with_optional_permit (acc, p : permit_return * permit_buy_param)  : permit_return = 
  let (ops, permit_storage) = acc in 
  let (purchaser, is_permited) : address * bool = match p.optional_permit with 
    | None -> (Tezos.sender, false)
    | Some permit -> 
        let u : unit = fail_if_not_admin(permit_storage.market_storage.admin) in
        let param_hash = Crypto.blake2b (Bytes.pack p.sale_id) in 
        let v : unit = check_permit (permit, permit_storage.counter, param_hash) in 
        (address_from_key permit.signerKey, true) in
  let buy_data : buy_data = {purchaser = purchaser; payment_relayer = Tezos.sender;} in
  let (maybe_op, market_storage) : operation option * storage 
    = buy_token_pending_confirmation (p.sale_id, buy_data, is_permited, permit_storage.market_storage) in
  let new_ops : operation list = (match maybe_op with 
      Some op -> op :: ops 
    | None -> ops) in 
  (new_ops, {permit_storage with market_storage = market_storage; counter = permit_storage.counter + 1n})

let buy_with_optional_permits (permits, permit_storage : permit_buy_param list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold buy_with_optional_permit permits acc)

let fixed_price_sale_permit_main (p, permit_storage : offline_market_entry_points * permit_storage) : operation list * permit_storage = 
  let storage = permit_storage.market_storage in 
  match p with
    | BaseSale entrypoints -> 
        let ops, market_storage = fixed_price_sale_without_buy(entrypoints, storage) in 
        ops, {permit_storage with market_storage = market_storage}
    | Permit_buy permits ->
       let u : unit = fail_if_paused(storage.admin) in
#if FEE
       let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       buy_with_optional_permits(permits, permit_storage)
    
    | Confirm_purchases pending_purchases -> 
       let u : unit = fail_if_paused(storage.admin) in
#if FEE
       let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       let w : unit = fail_if_not_admin(storage.admin) in
       confirm_purchases(pending_purchases, permit_storage)
    | Revoke_purchases pending_purchases -> 
       let u : unit = fail_if_paused(storage.admin) in
#if FEE
       let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       let w : unit = fail_if_not_admin(storage.admin) in
       revoke_purchases(pending_purchases, permit_storage)

(*VIEWS*)
let rec activeSalesHelper (active_sales, sale_id, s : (sale list) * sale_id * permit_storage) 
  : (sale list) = 
  (if sale_id >= s.market_storage.next_sale_id 
  then active_sales
  else ( match (Big_map.find_opt sale_id s.market_storage.sales) with 
    | Some sale -> activeSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveSales (initial_sale_id , s : sale_id * permit_storage) : (sale list) = 
  (activeSalesHelper (([] : sale list), initial_sale_id,  s))

#endif