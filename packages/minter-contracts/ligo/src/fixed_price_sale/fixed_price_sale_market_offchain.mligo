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

let execute_pending_purchase (acc, pending_purchase: permit_return * pending_purchase ) : permit_return =
  let (oplist, permit_storage) = acc in 
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        purchaser = purchaser; } = pending_purchase in  
  let sale : sale = get_sale(sale_id, storage) in

  let pending_purchases = sale.pending_purchases in 
  let token_for_sale_token_id = sale.sale_data.sale_token.token_id in 
  let token_for_sale_address = sale.sale_data.sale_token.fa2_address in 
  let amount_ = sale.sale_data.amount in 
  
  let u : unit = assert_msg(Set.mem purchaser pending_purchases, "PURCHASE_NOT_FOUND") in 
  let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, 1n , Tezos.self_address, purchaser) in
  let oplist = tx_nft :: oplist in 
  let new_sales : (sale_id, sale) big_map = 
    if amount_ <= 0n && (Set.size pending_purchases <= 1n) 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with  
      pending_purchases = Set.remove purchaser pending_purchases}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  (oplist, new_s)

let confirm_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold execute_pending_purchase pending_purchases acc)

let revoke_pending_purchase (permit_storage, pending_purchase: permit_storage * pending_purchase ) : permit_storage =
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        purchaser = purchaser; } = pending_purchase in  
  let sale : sale = get_sale(sale_id, storage) in

  let pending_purchases = sale.pending_purchases in 
  let amount_ = sale.sale_data.amount in  

  let u : unit = assert_msg(Set.mem purchaser pending_purchases, "PURCHASE_NOT_FOUND") in 

  let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some {sale with sale_data = {sale.sale_data with amount = amount_ + 1n};
                                              pending_purchases = Set.remove purchaser pending_purchases}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  new_s

let revoke_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let new_s : permit_storage = (List.fold revoke_pending_purchase pending_purchases permit_storage) in 
  ([] : operation list), new_s

let buy_token_pending_confirmation (sale_id, purchaser, storage: sale_id * address * storage) : storage = begin 
    let sale : sale = get_sale(sale_id, storage) in
    
    let pending_purchases = sale.pending_purchases in 
    let amount_ = sale.sale_data.amount in  

    assert_msg(amount_ >= 1n, "NO_SALE");
  
    assert_msg(not (Set.mem purchaser pending_purchases), "PENDING_PURCHASE_PRESENT"); 
  
    let new_sales : (sale_id, sale) big_map = 
      Big_map.update sale_id (Some {sale with pending_purchases = (Set.add purchaser pending_purchases); 
                                   sale_data = {sale.sale_data with amount = abs (amount_ - 1n)}}) storage.sales in
    let new_s = {storage with sales = new_sales } in
    new_s
  end 

let buy_with_permit (permit_storage, p : permit_storage * permit_buy_param)  : permit_storage = 
  let param_hash = Crypto.blake2b (Bytes.pack p.sale_id) in 
  let v : unit = check_permit (p.permit, permit_storage.counter, param_hash) in 
  let purchaser = address_from_key (p.permit.signerKey) in
  let market_storage : storage 
    = buy_token_pending_confirmation (p.sale_id, purchaser, permit_storage.market_storage) in
  {permit_storage with market_storage = market_storage; counter = permit_storage.counter + 1n}

let buy_with_permits (permits, permit_storage : permit_buy_param list * permit_storage) : permit_return =  
  let new_s : permit_storage = (List.fold buy_with_permit permits permit_storage) in 
  ([] : operation list), new_s

let fixed_price_sale_permit_main (p, permit_storage : offline_market_entry_points * permit_storage) : operation list * permit_storage = 
  let storage = permit_storage.market_storage in 
  match p with
    | BaseSale entrypoints -> 
        let ops, market_storage = fixed_price_sale_main(entrypoints, storage) in 
        ops, {permit_storage with market_storage = market_storage}
    | Offchain_buy permits ->
       let u : unit = fail_if_paused(storage.admin) in
#if FEE
       let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       let w : unit = fail_if_not_admin(storage.admin) in
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