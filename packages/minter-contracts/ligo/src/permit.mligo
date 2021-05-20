#if !PERMIT
#define PERMIT

#include "common.mligo"

type permit = 
  [@layout:comb]
  {
    signerKey: key;
    signature: signature;
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

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

let check_permit (p, counter, param_hash : permit * nat * bytes) : unit = begin 
    (* let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (counter, param_hash) : bytes) in *)
    let unsigned : bytes = Bytes.pack ((Tezos.chain_id, Tezos.self_address), (counter, param_hash)) in
    assert_msg (Crypto.check p.signerKey p.signature unsigned, "MISSIGNED")
  end 

#endif