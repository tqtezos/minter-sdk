(*TYPES*)

type fee_data = 
  [@layout:comb]
  {
    fee_address : address;
    fee_percent : nat;
  }

type fa2_token =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }
type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_token list);
  }


(*MATH*) 

let ceil_div_nat (numerator, denominator : nat * nat) : nat = abs ((- numerator) / (int denominator))

let percent_of_bid_nat (percent, bid : nat * nat) : nat = 
  (ceil_div_nat (bid *  percent, 100n))

let ceil_div_tez (tz_qty, nat_qty : tez * nat) : tez = 
  let ediv1 : (tez * tez) option = ediv tz_qty nat_qty in 
  match ediv1 with 
    | None -> (failwith "DIVISION_BY_ZERO"  : tez) 
    | Some e -> 
       let (quotient, remainder) = e in
       if remainder > 0mutez then (quotient + 1mutez) else quotient

let percent_of_bid_tez (percent, bid : nat * tez) : tez = 
  (ceil_div_tez (bid *  percent, 100n))


(*HELPERS*)

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let address_to_contract_transfer_entrypoint(add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : (transfer list) contract)
  | Some c ->  c

let resolve_contract (add : address) : unit contract =
  match ((Tezos.get_contract_opt add) : (unit contract) option) with
      None -> (failwith "Return address does not resolve to contract" : unit contract)
    | Some c -> c
