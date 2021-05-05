#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../common.mligo"

type sale_param_tez =
[@layout:comb]
{
  sale_token: global_token_id;
  price: tez;
}

type sale_tez =
[@layout:comb]
{
  seller: address;
  sale : sale_param_tez;
}

#if !FEE 

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  next_sale_id : sale_id;
  sales: (sale_id, sale_tez) big_map;
}

#else 

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  next_sale_id : sale_id;
  sales: (sale_id, sale_tez) big_map;
  fee : fee_data;
}

#endif 

type market_entry_points =
  | Sell of sale_param_tez
  | Buy of sale_id
  | Cancel of sale_id
  | Admin of pauseable_admin

let transfer_nft(fa2_address, token_id, from, to_: address * token_id * address * address): operation =
  let fa2_transfer : ((transfer list) contract) option =
      Tezos.get_entrypoint_opt "%transfer"  fa2_address in
  let transfer_op = match fa2_transfer with
  | None -> (failwith "CANNOT_INVOKE_FA2_TRANSFER" : operation)
  | Some c ->
    let tx = {
      from_ = from;
      txs= [{
        to_ = to_;
        token_id = token_id;
        amount = 1n;
    }]} in
    Tezos.transaction [tx] 0mutez c in
  transfer_op

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination = (match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
    | Some acc -> acc) in 
  Tezos.transaction () qty destination

let buy_token(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let sale_data : sale_tez = (match Big_map.find_opt sale_id storage.sales with
  | None -> (failwith "NO_SALE": sale_tez)
  | Some s -> s) in 
  let sale_price : tez = sale_data.sale.price in
  let amountError : unit =
    if Tezos.amount <> sale_price
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", sale_price, Tezos.amount) : unit)
    else () in
  let tx_nft = transfer_nft(sale_data.sale.sale_token.fa2_address, sale_data.sale.sale_token.token_id, Tezos.self_address, Tezos.sender) in
#if !FEE 
  let tx_price = transfer_tez(sale_price, sale_data.seller) in
  let oplist : operation list = [tx_nft; tx_price] in 
#else 
  let fee : tez = percent_of_bid_tez (storage.fee.fee_percent, sale_price) in
  let sale_price_minus_fee : tez = sale_price - fee in
  let tx_fee : operation = transfer_tez(fee, storage.fee.fee_address) in
  let tx_price = transfer_tez(sale_price_minus_fee, sale_data.seller) in
  let oplist : operation list = [tx_price; tx_nft; tx_fee] in
#endif 
  let new_s = { storage with sales = Big_map.remove sale_id storage.sales } in
  oplist, new_s

let tez_stuck_guard(entrypoint: string) : string = "DON'T TRANSFER TEZ TO THIS ENTRYPOINT (" ^ entrypoint ^ ")"

let deposit_for_sale(sale, storage: sale_param_tez * storage) : (operation list * storage) =
    let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "SELL") else () in
    let transfer_op =
      transfer_nft (sale.sale_token.fa2_address, sale.sale_token.token_id, Tezos.sender, Tezos.self_address) in
    let sale_data = { seller = Tezos.sender; sale = sale } in
    let sale_id : sale_id = storage.next_sale_id in 
    let new_s = { storage with sales = Big_map.add sale_id sale_data storage.sales; 
      next_sale_id =  sale_id + 1n; } in
    [transfer_op], new_s

let cancel_sale(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "CANCEL") else () in
  match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE" : (operation list * storage))
    | Some sale_data -> let is_seller = Tezos.sender = sale_data.seller in
                        let v : unit = if is_seller then ()
                          else fail_if_not_admin_ext (storage.admin, "OR A SELLER") in
                        let tx_nft_back_op = transfer_nft(sale_data.sale.sale_token.fa2_address, sale_data.sale.sale_token.token_id, Tezos.self_address, sale_data.seller) in
                        [tx_nft_back_op], {storage with sales = Big_map.remove sale_id storage.sales }

let fixed_price_sale_tez_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sale ->
     let u : unit = fail_if_paused(storage.admin) in
#if FEE
     let v : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_PERCENT_TOO_HIGH") in
#endif
     let w : unit = fail_if_not_admin(storage.admin) in
     deposit_for_sale(sale, storage)
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

#if !FEE

let sample_storage : storage =
  {
    admin = Some ({
        admin =  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
        pending_admin = (None : address option);
        paused = false;});
    sales = (Big_map.empty : (sale_id, sale_tez) big_map);
    next_sale_id = 0n;
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
    fee = {
      fee_address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
      fee_percent = 15n;
    }
  }

#endif 