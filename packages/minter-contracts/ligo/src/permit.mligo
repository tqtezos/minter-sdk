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

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

(*param_hash is the Blake2b of the hashed parameter*)
let check_permit (p, counter, param_hash : permit * nat * bytes) : unit = 
    (* let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (counter, param_hash) : bytes) in *)
    let unsigned : bytes = Bytes.pack ((Tezos.chain_id, Tezos.self_address), (counter, param_hash)) in
    let permit_valid : bool = Crypto.check p.signerKey p.signature unsigned in 
    let u : unit = (if not permit_valid 
     then ([%Michelson ({| { FAILWITH } |} : string * bytes -> unit)] ("MISSIGNED", unsigned) : unit)
     else ()) in 
    u
    
#endif