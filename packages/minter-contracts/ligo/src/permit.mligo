#if !PERMIT
#define PERMIT

#include "common.mligo"

type permit = 
  [@layout:comb]
  {
    signerKey: key;
    signature: signature;
  }

let address_from_key (key : key) : address =
  let a = Tezos.address (Tezos.implicit_account (Crypto.hash_key key)) in
  a

let check_permit (p, counter, param_hash : permit * nat * bytes) : unit = begin 
    (* let unsigned : bytes = ([%Michelson ({| { SELF; ADDRESS; CHAIN_ID; PAIR; PAIR; PACK } |} : nat * bytes -> bytes)] (counter, param_hash) : bytes) in *)
    let unsigned : bytes = Bytes.pack ((Tezos.chain_id, Tezos.self_address), (counter, param_hash)) in
    assert_msg (Crypto.check p.signerKey p.signature unsigned, "MISSIGNED")
  end 

#endif