type permit_storage = 
    [@layout:comb]
  {
    market_storage : storage;
    counter : nat;
  }

type permit_return = (operation list) * permit_storage

type offline_market_entry_points =
  | BaseSale of market_entry_points
  | Offchain_buy of permit_buy_param list 
  | Confirm_purchases of pending_purchases
  | Revoke_purchases of pending_purchases

let confirm_purchases_for_sale (acc, pending_purchase: permit_return * pending_purchase ) : permit_return =
  let { sale_id = sale_id; 
        purchases = purchases; } = pending_purchase in  
  let (oplist, permit_storage) = acc in 
  let storage = permit_storage.market_storage in 

  let sale : sale = get_sale(sale_id, storage) in

  let token_for_sale_token_id = sale.sale_data.sale_token.token_id in 
  let token_for_sale_address = sale.sale_data.sale_token.fa2_address in 
  let amount_ = sale.sale_data.amount in 
  
  let new_txs_and_sale : (operation list * sale) = 
    List.fold (fun (txs_with_sale, ppid : (operation list * sale) * pending_purchase_id) -> 
      let (ops, s) : (operation list * sale) = txs_with_sale in 
      (match (Map.find_opt ppid s.pending_purchases) with
          None -> (failwith "PURCHASE_NOT_FOUND" : (operation list * sale))
        | Some p -> ( let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, p.amount , Tezos.self_address, p.purchaser) in
                      ((tx_nft :: ops), 
                       {s with pending_purchases = (Map.remove ppid s.pending_purchases);}
                      ) : (operation list * sale)
                    )
      )  
    ) purchases (oplist, sale) in

  let (oplist, sale_record) = new_txs_and_sale in 

  let new_sales : (sale_id, sale) big_map = 
    if amount_ <= 0n && (Map.size sale_record.pending_purchases <= 0n) 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some sale_record) storage.sales in
  
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales}} in
  (oplist, new_s)

let confirm_purchases (pending_purchases, permit_storage : pending_purchases * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold confirm_purchases_for_sale pending_purchases acc)

let revoke_purchases_for_sale (permit_storage, pending_purchase: permit_storage * pending_purchase ) : permit_storage =
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        purchases = purchases;
      } = pending_purchase in  
  let sale : sale = get_sale(sale_id, storage) in

  let new_sale = List.fold (fun (s, ppid : sale * pending_purchase_id) -> 
     (match (Map.find_opt ppid s.pending_purchases) with
         None -> (failwith "PURCHASE_NOT_FOUND" : sale)
       | Some p -> ({ s with pending_purchases = Map.remove ppid s.pending_purchases;
                             sale_data = {s.sale_data with amount = s.sale_data.amount + p.amount};
                    } : sale)
     )  
    ) purchases sale in 
         
  let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some new_sale) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  new_s

let revoke_purchases (pending_purchases, permit_storage : pending_purchases * permit_storage) : permit_return = 
  let new_s : permit_storage = (List.fold revoke_purchases_for_sale pending_purchases permit_storage) in 
  ([] : operation list), new_s

let buy_token_pending_confirmation (buy_param, purchaser, storage: buy_param * address * storage) : storage = begin 
    let { sale_id = sale_id;
          buy_amount = buy_amount } = buy_param in 
    let sale : sale = get_sale(sale_id, storage) in

    let sale_price = sale.sale_data.price in 
    let amount_ = sale.sale_data.amount in 
    let next_pending_purchase_id = sale.next_pending_purchase_id in 

    let new_amount : int = amount_ - buy_amount in 
    let u : unit = assert_msg(new_amount >= 0, "BUY_AMOUNT_EXCEEDS_REMAINING_SUPPLY") in
    let purchase_data : purchase_data = {
      purchaser = purchaser;
      amount = buy_amount;} in 
    let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some {sale with pending_purchases = (Map.update next_pending_purchase_id (Some purchase_data) sale.pending_purchases); 
                                   sale_data = {sale.sale_data with amount = abs (new_amount)};
                                   next_pending_purchase_id = next_pending_purchase_id + 1n;}) storage.sales in
    let new_s = {storage with sales = new_sales } in
    new_s
  end 

let buy_with_permit (permit_storage, p : permit_storage * permit_buy_param)  : permit_storage = 
  let param_hash = Crypto.blake2b (Bytes.pack p.buy_param) in 
  let u : unit = check_permit (p.permit, permit_storage.counter, param_hash) in 
  let purchaser : address = address_from_key (p.permit.signerKey) in
  let market_storage = buy_token_pending_confirmation (p.buy_param, purchaser, permit_storage.market_storage) in
  {permit_storage with market_storage = market_storage; counter = permit_storage.counter + 1n}

let buy_with_permits (permits, permit_storage : permit_buy_param list * permit_storage) : permit_return = 
  let new_s = (List.fold buy_with_permit permits permit_storage) in 
  ([] : operation list), new_s

let fixed_price_sale_permit_main (p, permit_storage : offline_market_entry_points * permit_storage) : operation list * permit_storage = 
  let storage = permit_storage.market_storage in 
  match p with
    | BaseSale entrypoints -> 
        let ops, market_storage = fixed_price_sale_main(entrypoints, storage) in 
        ops, {permit_storage with market_storage = market_storage}
    | Offchain_buy permits ->
       let u : unit = fail_if_paused(storage.admin) in
       let v : unit = fail_if_not_admin(storage.admin) in
#if FEE
       let w : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       buy_with_permits(permits, permit_storage)
    
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