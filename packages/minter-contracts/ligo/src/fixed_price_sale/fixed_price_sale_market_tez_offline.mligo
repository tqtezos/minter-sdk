#if !PERMIT_MARKET
#define PERMIT_MARKET

#include "fixed_price_sale_market_tez.mligo"
#include "../permit.mligo"

type permit_storage = 
    [@layout:comb]
  {
    market_storage : storage;
    counter : nat;
  }

type permit_buy_param =
  [@layout:comb]
  {
    sale_id : sale_id;
    optional_permit : permit option;
  } 

type pending_purchase = 
  [@layout:comb]
  {
    sale_id : sale_id;
    purchaser : address;
  }

type pending_purchases = pending_purchase list 

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
        purchaser = purchaser; } = pending_purchase in  
  let sale : sale_tez = match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE": sale_tez)
    | Some s -> s
  in
  let { seller = seller;
        pending_purchasers = pending_purchasers;
        sale_data = {
            sale_token = {
                token_id = token_for_sale_token_id;
                fa2_address = token_for_sale_address;
            };
            price = sale_price;
            amount = amount_;
        }

  } = sale in 
  
  let u : unit = assert_msg(Set.mem purchaser pending_purchasers, "PURCHASER_NOT_FOUND") in 
  let tx_nft = transfer_fa2(token_for_sale_address, token_for_sale_token_id, 1n , Tezos.self_address, purchaser) in
#if !FEE 
  let tx_price = transfer_tez(sale_price, seller) in
  let oplist : operation list = tx_price :: tx_nft :: oplist in 
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
    if amount_ <= 0n && (Set.size pending_purchasers <= 1n) 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with  
      pending_purchasers = Set.remove purchaser pending_purchasers}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  (oplist, new_s)

let confirm_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold execute_pending_purchase pending_purchases acc)

let revoke_pending_purchase (acc, pending_purchase: permit_return * pending_purchase ) : permit_return =
  let (oplist, permit_storage) = acc in 
  let storage = permit_storage.market_storage in 
  let { sale_id = sale_id; 
        purchaser = purchaser; } = pending_purchase in  
  let sale : sale_tez = match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE": sale_tez)
    | Some s -> s
  in
  let { seller = seller;
        pending_purchasers = pending_purchasers;
        sale_data = {
            sale_token = {
                token_id = token_for_sale_token_id;
                fa2_address = token_for_sale_address;
            };
            price = sale_price;
            amount = amount_;
        }

  } = sale in 
  let u : unit = assert_msg(Set.mem purchaser pending_purchasers, "PURCHASER_NOT_FOUND") in 
  (*Multi-Admin case??*)
  let tx_price = transfer_tez(sale_price, Tezos.sender) in
  let oplist : operation list = tx_price :: oplist in 
  let new_sales : (sale_id, sale_tez) big_map = 
      Big_map.update sale_id (Some {sale with sale_data = {sale.sale_data with amount = amount_ + 1n};
                                              pending_purchasers = Set.remove purchaser pending_purchasers}) storage.sales in
  let new_s = {permit_storage with market_storage = {storage with sales = new_sales }} in
  (oplist, new_s)

let revoke_purchases (pending_purchases, permit_storage : pending_purchase list * permit_storage) : permit_return = 
  let acc : permit_return = ([] : operation list), permit_storage in 
  (List.fold revoke_pending_purchase pending_purchases acc)

let buy_token_pending_confirmation (sale_id, purchaser, storage: sale_id * address * storage) : storage = begin 
    let sale : sale_tez = match Big_map.find_opt sale_id storage.sales with
      | None -> (failwith "NO_SALE": sale_tez)
      | Some s -> s
    in
    let sale_price = sale.sale_data.price in
    let amount_ = sale.sale_data.amount in 
    let pending_purchasers = sale.pending_purchasers in 
    assert_msg(amount_ >= 1n, "NO_SALE");
    let new_sales : (sale_id, sale_tez) big_map = 
      Big_map.update sale_id (Some {sale with pending_purchasers = (Set.add purchaser pending_purchasers); 
                                   sale_data = {sale.sale_data with amount = abs (amount_ - 1n)}}) storage.sales in
    let new_s = {storage with sales = new_sales } in
    new_s
  end 

let buy_with_optional_permit (permit_storage, p : permit_storage * permit_buy_param)  : permit_storage = 
  let purchaser = match p.optional_permit with 
    | None -> Tezos.sender
    | Some permit -> 
        let param_hash = Crypto.blake2b (Bytes.pack p.sale_id) in 
        let u : unit = check_permit (permit, permit_storage.counter, param_hash) in 
        address_from_key permit.signerKey in
  let market_storage = buy_token_pending_confirmation (p.sale_id, purchaser, permit_storage.market_storage) in
  {permit_storage with market_storage = market_storage; counter = permit_storage.counter + 1n}

let buy_with_optional_permits (permits, permit_storage : permit_buy_param list * permit_storage) : permit_return = 
  let total_owed : tez = (List.fold (fun (acc, p : tez * permit_buy_param) -> 
    let sale : sale_tez = match Big_map.find_opt p.sale_id permit_storage.market_storage.sales with
      | None -> (failwith "NO_SALE": sale_tez)
      | Some s -> s
    in
    let sale_price = sale.sale_data.price in
    acc + sale_price) permits 0mutez) in 
  let amountError : unit =
    if Tezos.amount <> total_owed
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", total_owed, Tezos.amount) : unit)
    else () in
  let new_s = (List.fold buy_with_optional_permit permits permit_storage) in 
  ([] : operation list), new_s

let fixed_price_sale_permit_main (p, permit_storage : offline_market_entry_points * permit_storage) : operation list * permit_storage = 
  let storage = permit_storage.market_storage in 
  match p with
    | BaseSale entrypoints -> 
        let ops, market_storage = fixed_price_sale_tez_without_buy(entrypoints, storage) in 
        ops, {permit_storage with market_storage = market_storage}
    | Permit_buy permits ->
       let u : unit = fail_if_paused(storage.admin) in
#if FEE
       let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_TOO_HIGH") in
#endif
       let w : unit = fail_if_not_admin(storage.admin) in
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
let rec activeSalesHelper (active_sales, sale_id, s : (sale_tez list) * sale_id * permit_storage) 
  : (sale_tez list) = 
  (if sale_id >= s.market_storage.next_sale_id 
  then active_sales
  else ( match (Big_map.find_opt sale_id s.market_storage.sales) with 
    | Some sale -> activeSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveSales (initial_sale_id , s : sale_id * permit_storage) : (sale_tez list) = 
  (activeSalesHelper (([] : sale_tez list), initial_sale_id,  s))

#endif