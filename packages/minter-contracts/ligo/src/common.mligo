#if !COMMON
#define COMMON

#include "../fa2/fa2_interface.mligo"

(*TYPES*)

type sale_id = nat

type fee_data =
  [@layout:comb]
  {
    fee_address : address;
    fee_percent : nat;
  }

type fa2_tokens =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }
type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_tokens list);
  }

type global_token_id =
  [@layout:comb]
  {
      fa2_address : address;
      token_id : token_id;
  }

(*MATH*)

(*In English auction it is necessary to use ceiling so that bid is guaranteed to be raised*)
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

(*For fee calculations in Auction/Fixed-Price, normal division is used*)

let percent_of_price_tez (percent, price : nat * tez) : tez =
  ((price * percent)/ 100n)

let percent_of_price_nat (percent, price : nat * nat) : nat =
  ((price * percent)/ 100n)

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

let transfer_fa2(fa2_address, token_id, amount_, from, to_: address * token_id * nat * address * address): operation =
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
        amount = amount_;
    }]} in
    Tezos.transaction [tx] 0mutez c
 in transfer_op

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination = (match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
    | Some acc -> acc) in
  Tezos.transaction () qty destination

let check_tokens_allowed
    (tokens, allowlist, err : tokens * allowlist * string) : unit =

#if !ALLOWLIST_ENABLED
  unit

#elif ALLOWLIST_SIMPLE
  check_address_allowed(tokens.fa2_address, allowlist, err)

#elif ALLOWLIST_TOKEN
  begin match Big_map.find_opt tokens.fa2_address allowlist with
  | None -> failwith err
  | Some m_tokens_allowlist -> begin match m_tokens_allowlist with
    | All_token_ids_allowed -> unit
    | Token_ids_allowed token_ids_allowlist ->
        List.iter
          (fun (token : fa2_tokens) ->
            if Set.mem token.token_id token_ids_allowlist
              then unit
              else failwith err
          )
          tokens.fa2_batch
    end
  end

#else
<No check_tokens_allowed implementation>
#endif

#endif
