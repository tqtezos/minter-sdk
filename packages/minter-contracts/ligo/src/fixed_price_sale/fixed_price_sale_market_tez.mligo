#include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/pauseable_admin_option.mligo"
#include "../common.mligo"

type sale_token_param_tez =
[@layout:comb]
{
 token_for_sale_address: address;
 token_for_sale_token_id: token_id;
}

type sale_param_tez =
[@layout:comb]
{
  seller: address;
  sale_token: sale_token_param_tez;
}

#if !FEE 

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  sales: (sale_param_tez, tez) big_map;
}

#else 

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  sales: (sale_param_tez, tez) big_map;
  fee : fee_data;
}

#endif 

type init_sale_param_tez =
[@layout:comb]
{
  sale_price: tez;
  sale_token_param_tez: sale_token_param_tez;
}

type market_entry_points =
  | Sell of init_sale_param_tez
  | Buy of sale_param_tez
  | Cancel of sale_param_tez
  | Admin of pauseable_admin

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

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

let buy_token(sale, storage: sale_param_tez * storage) : (operation list * storage) =
  let sale_price : tez = (match Big_map.find_opt sale storage.sales with
  | None -> (failwith "NO_SALE": tez)
  | Some s -> s) in 
  let amountError : unit =
    if Tezos.amount <> sale_price
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", sale_price, Tezos.amount) : unit)
    else () in
  let tx_nft = transfer_nft(sale.sale_token.token_for_sale_address, sale.sale_token.token_for_sale_token_id, Tezos.self_address, Tezos.sender) in
#if !FEE 
  let tx_price = transfer_tez(sale_price, sale.seller) in
  let oplist : operation list = [tx_nft; tx_price] in 
#else 
  let fee : tez = percent_of_bid_tez (storage.fee.fee_percent, sale_price) in
  let sale_price_minus_fee : tez = sale_price - fee in
  let tx_fee : operation = transfer_tez(fee, storage.fee.fee_address) in
  let tx_price = transfer_tez(sale_price_minus_fee, sale.seller) in
  let oplist : operation list = [tx_price; tx_nft; tx_fee] in
#endif 
  let new_s = { storage with sales = Big_map.remove sale storage.sales } in
  oplist, new_s

let tez_stuck_guard(entrypoint: string) : string = "DON'T TRANSFER TEZ TO THIS ENTRYPOINT (" ^ entrypoint ^ ")"

let deposit_for_sale(sale_token, price, storage: sale_token_param_tez * tez * storage) : (operation list * storage) =
    let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "SELL") else () in
    let transfer_op =
      transfer_nft (sale_token.token_for_sale_address, sale_token.token_for_sale_token_id, Tezos.sender, Tezos.self_address) in
    let sale_param = { seller = Tezos.sender; sale_token = sale_token } in
    let new_s = { storage with sales = Big_map.add sale_param price storage.sales } in
    [transfer_op], new_s

let cancel_sale(sale, storage: sale_param_tez * storage) : (operation list * storage) =
  let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "CANCEL") else () in
  match Big_map.find_opt sale storage.sales with
    | None -> (failwith "NO_SALE" : (operation list * storage))
    | Some price -> if sale.seller = Tezos.sender then
                      let tx_nft_back_op = transfer_nft(sale.sale_token.token_for_sale_address, sale.sale_token.token_for_sale_token_id, Tezos.self_address, Tezos.sender) in
                      [tx_nft_back_op], {storage with sales = Big_map.remove sale storage.sales }
      else (failwith "NOT_OWNER": (operation list * storage))

let fixed_price_sale_tez_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sale ->
#if FEE
     let u : unit = assert_msg (storage.fee.fee_percent <= 100n, "FEE_PERCENT_TOO_HIGH") in
#endif
     let v : unit = fail_if_not_admin(storage.admin) in
     deposit_for_sale(sale.sale_token_param_tez, sale.sale_price, storage)
  | Buy sale ->
     let u : unit = fail_if_paused(storage.admin) in
     buy_token(sale, storage)
  | Cancel sale ->
     let is_seller = Tezos.sender = sale.seller in
     let u : unit = if is_seller then ()
             else fail_if_not_admin_ext (storage.admin, "OR A SELLER") in
     cancel_sale(sale,storage)
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
    sales = (Big_map.empty : (sale_param_tez, tez) big_map);
  }

#else 

let sample_storage : storage =
  {
    admin = Some ( {
        admin =  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
        pending_admin = (None : address option);
        paused = false;
              });
    sales = (Big_map.empty : (sale_param_tez, tez) big_map);
    fee = {
      fee_address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" :address);
      fee_percent = 15n;
    }
  }

#endif 