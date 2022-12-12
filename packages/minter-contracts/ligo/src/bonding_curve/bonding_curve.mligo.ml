// resolve_address
#include "../common.mligo"

// admin_storage
// admin_entrypoints
#include "../../fa2_modules/admin/simple_admin.mligo"

// fa2_entry_points
// token_metadata
#include "../../fa2/fa2_interface.mligo"

// mint_token_param
// mint_tokens_param
#include "../minter_collection/nft/fa2_multi_nft_manager.mligo"

// ////////////////////////////////////////////////////////////////
// ERRORS
// ////////////////////////////////////////////////////////////////

(**
  storage.unclaimed == 0
*)
[@inline]
let error_unclaimed_is_zero = "UNCLAIMED=0"

(**
  Wrong tez price sent when buying
*)
[@inline]
let error_wrong_tez_price = "WRONG_TEZ_PRICE"

(**
  run_piecewise_polynomial gave a negative cost
*)
[@inline]
let error_negative_cost = "NEGATIVE_COST"

(**
  market_contract address does not refer to a contract with a '%mint'
  entrypoint with type mint_tokens_param
*)
[@inline]
let error_no_mint_entrypoint = "NO_MINT"

(**
  market_contract address does not refer to a contract with a '%burn'
  entrypoint with type (token_id * bytes)
*)
[@inline]
let error_no_burn_entrypoint = "NO_BURN"

(**
  token_index = 0,
  i.e. no tokens have been sold to the bonding curve,
  i.e. there are no tokens to sell
*)
[@inline]
let error_no_token_to_sell = "NO_TOKENS"

(**
  "symbol" field not found in storage.token_metadata
*)
[@inline]
let error_token_metadata_symbol_missing = "NO_SYMBOL"

(**
  Can't return tez to the given seller address because it doesn't have a default
  entrypoint to send tez to
*)
[@inline]
let error_no_default_entrypoint = "CANT_RETURN"

(**
  Entrypoint is unimplemented
*)
[@inline]
let error_unimplemented_entrypoint = "UNIMPLEMENTED"

// ////////////////////////////////////////////////////////////////

// length of one of the segments in a piecewise_polynomial
type piecewise_length = nat

// A list of coefficients for a polynomial over the integers.
//
// See run_polynomial for more info.
type polynomial =
  [@layout:comb]
  {
    coefficients : int list;
  }

// Accumulator for run_polynomial
type polynomial_acc =
  {
    result : int;

    (** x^i for some i
    *)
    x_pow : int;
  }

// Run a polynomial [a0; a1; .. ; an] on an input 'x' as
// a0 * x^0 + a1 * x^1 + .. + an * x^n
[@inline]
let run_polynomial (poly, x : polynomial * int)
    : int =
    let output = List.fold_left
      (fun (poly_acc, coefficient : polynomial_acc * int) ->
        let x_pow = poly_acc.x_pow in
        let x_pow_next = x * x_pow in
        let output : polynomial_acc =
          {
            result = poly_acc.result + coefficient * x_pow;
            x_pow = x_pow_next;
          }
        in output
      )
      {
        result = 0;
        x_pow = 1;
      }
      poly.coefficients in
    output.result

// A segment of a piecewise function
type piecewise_segment = 
  {
    length : piecewise_length;
    poly : polynomial;
  }

// The 'piecewise_length' is the length of each segment
// and the formula for each segment is given by the associated 'polynomial'
//
// [ (length_0, polynomial_0); (length_1, polynomial_1); .. ]
//
// ->
//
// f(x) :=
//    { polynomial_0(x) | 0 <= x < length_0
//    { polynomial_1(x) | length_0 <= x < length_0 + length_1
//    ..
//    { polynomial_i(x) | sum_{0 <= j <= i-1} length_j <= x < sum_{0 <= j <= i} length_j
//    ..
//    { polynomial_last(x) | sum_{0 <= j < last-1} length_j <= x
type piecewise_polynomial =
  [@layout:comb]
  {
    segments : piecewise_segment list;
    last_segment : polynomial;
  }

// Accumulator for run_piecewise_polynomial
type piecewise_polynomial_acc =
  {
    // Current segment offset, i.e. sum of piecewise_length's up to the current
    // location in piecewise_polynomial.segments
    offset : nat;

    // The input was found in this polynomial when Some
    in_poly : polynomial option
  }

// Run a piecewise polynomial by finding the segment for the current offset and
// calling run_polynomial
//
// Given all of the piecewise_length's as a list piecewise_lengths, the current
// segment can be considered the unique (n) for which the following holds:
//   sum (take n piecewise_lengths) <= x < sum (take (n+1) piecewise_lengths)
// Or else the 'last_segment'
let run_piecewise_polynomial (piecewise_poly, x : piecewise_polynomial * nat)
    : int =
    let output : piecewise_polynomial_acc = List.fold_left
      (fun (piecewise_acc, segment : piecewise_polynomial_acc * piecewise_segment) ->
        match piecewise_acc.in_poly with
        | Some poly -> piecewise_acc
        | None ->
            let offset_next : nat = piecewise_acc.offset + segment.length in
            if x <= offset_next
            then {piecewise_acc with in_poly = Some segment.poly}
            else {piecewise_acc with offset = offset_next}
      )
      {
        offset = 0n;
        in_poly = (None : polynomial option);
      }
      piecewise_poly.segments in

    let x_in_poly : polynomial = (
      match output.in_poly with
      | Some poly -> poly
      | None -> piecewise_poly.last_segment) in
    run_polynomial(x_in_poly, int x)

// ////////////////////////////////////////////////////////////////



(** Tez used as a price *)
type price_tez = tez

(** Tez unclaimed that can be withdrawn *)
type unclaimed_tez = tez

type bonding_curve_storage =
  [@layout:comb]
  {
    admin : admin_storage;

    // fa2_entry_points contract
    market_contract : address;

    // final price of the auction
    // set this price constant based on final price of auction
    auction_price : tez;

    // TODO: auction_tokens_sold is unused!!!!
    // number of tokens sold _during_ the auction
    auction_tokens_sold : nat;

    // number of tokens sold _after_ the auction
    token_index : nat;

    // token metadata for minting
    token_metadata : token_metadata;

    // the percentage (in basis points) cost of buying and selling a token at the same index
    basis_points : nat;

    // bonding curve formula
    cost_mutez : piecewise_polynomial;

    // unclaimed tez (i.e. the result of the `basis_points` fee)
    unclaimed : tez;
  }

// Parameters to buy a single NFT from the bonding curve
type buy_order =
  [@layout:comb]
  {
    buy_order_contents : unit;
  }

// Parameters for selling a single NFT from the bonding curve
type sell_order = token_id
(*   [@layout:comb] *)
(*   { *)
(*     sell_order_contents : token_id; *)
(*   } *)

// alias for user receiving an NFT through a call to the Buy_offchain entrypoint
type offchain_buyer = address

// alias for user receiving an NFT through a call to the Sell_offchain entrypoint
type offchain_seller = address

type bonding_curve_entrypoints =
  | Admin of admin_entrypoints

  // update staking (admin only)
  | Set_delegate of key_hash option

  // withdraw profits or fail
  (* | Withdraw of tez *)
  (* | Withdraw of unclaimed_tez *)
  | Withdraw of unit

  // buy single token on-chain (requires tez deposit)
  | Buy of buy_order

  // buy tokens off-chain (admin only, requires tez deposit)
  | Buy_offchain of offchain_buyer

  // sell token on-chain (returns tez deposit)
  | Sell of sell_order

  // sell single/multi tokens off-chain (returns tez deposit)
  | Sell_offchain of (sell_order * offchain_seller)


// Debug-only
#if DEBUG_BONDING_CURVE

  // nat -> price in mutez of next token
  | Cost of nat

#endif // DEBUG_BONDING_CURVE


(** 10,000 basis points per 1 *)
[@inline]
let basis_points_per_unit : nat = 10000n

(** Buy single token on-chain (requires tez deposit)
* calculate current price from index and price constant (run_piecewise_polynomial)
* ensure sent tez = current price + basis_points
* mint token -> user -> market contract
  next token minted same as last?
* increment current token index
* update 'unclaimed'
*)
let buy_offchain_no_admin (buyer_addr, storage : offchain_buyer * bonding_curve_storage)
    : (operation list) * bonding_curve_storage =
    (* cost = auction_price + cost_mutez(token_index) + basis_point_fee *)
    let cost_tez : price_tez = match is_nat (run_piecewise_polynomial(storage.cost_mutez, storage.token_index)) with
    | None -> (failwith error_negative_cost : tez)
    | Some nat_cost_tez -> 1mutez * nat_cost_tez
    in let current_price : price_tez = storage.auction_price + cost_tez
    in let basis_point_fee : tez =
        (current_price * storage.basis_points) / basis_points_per_unit in

    (* assert cost = sent tez *)
    if Tezos.amount <> (current_price + basis_point_fee)
    then (failwith error_wrong_tez_price : (operation list) * bonding_curve_storage)
    else
      (* mint using storage.token_metadata *)
      let mint_entrypoint_opt : (mint_tokens_param contract) option =
          Tezos.get_entrypoint_opt "%mint" storage.market_contract in
      let mint_op : operation = match mint_entrypoint_opt with
      | None -> (failwith error_no_mint_entrypoint : operation)
      | Some contract_ref ->
          let mint_token_params : mint_token_param = {
            token_metadata = storage.token_metadata;
            owner = buyer_addr;
          }
          in Tezos.transaction [mint_token_params] 0mutez contract_ref
      in [mint_op], { storage with
                      token_index = storage.token_index + 1n;
                      unclaimed = storage.unclaimed + basis_point_fee }


(** Sell token (returns tez deposit)
- calculate _previous_ price
- burn token -> market contract
- return tez (sans basis_point_fee) to seller
- decrement current token_index in storage
*)
let sell_offchain_no_admin ((token_to_sell, seller_addr), storage : (token_id * offchain_seller) * bonding_curve_storage)
    : (operation list) * bonding_curve_storage =
    (* - previous_token_index = storage.token_index - 1n *)
    (* - if not is_nat previous_token_index, fail *)
    (* - cost_tez = run_piecewise_polynomial(.., previous_token_index) *)
    (* - current_price = storage.auction_price + cost_tez *)
    let previous_token_index : nat = match is_nat (storage.token_index - 1n) with
    | None -> (failwith error_no_token_to_sell : nat)
    | Some token_index -> token_index
    in
    let previous_cost_tez : price_tez = match is_nat (run_piecewise_polynomial(storage.cost_mutez, previous_token_index)) with
    | None -> (failwith error_negative_cost : tez)
    | Some nat_cost_tez -> storage.auction_price + 1mutez * nat_cost_tez
    (* - burn token -> market contract *)
    (* - send -> market contract *)
    in let burn_entrypoint_opt : ((token_id * bytes) contract) option =
      Tezos.get_entrypoint_opt "%burn" storage.market_contract
    in 

    let token_to_sell_symbol : bytes =
      match Map.find_opt "symbol" storage.token_metadata.token_info with
      | None -> (failwith error_token_metadata_symbol_missing : bytes)
      | Some token_to_sell_symbol -> token_to_sell_symbol
    in

    let burn_op : operation = match burn_entrypoint_opt with
    | None -> (failwith error_no_burn_entrypoint : operation)
    | Some contract_ref ->
        Tezos.transaction (token_to_sell, token_to_sell_symbol) 0mutez contract_ref
    in let return_tez_entrypoint : (unit contract) option =
      Tezos.get_contract_opt seller_addr
    in let return_tez_op : operation = match return_tez_entrypoint with
    | None -> (failwith error_no_default_entrypoint : operation)
    | Some seller_contract_ref ->
        Tezos.transaction unit previous_cost_tez seller_contract_ref
    in [burn_op; return_tez_op], { storage with token_index = previous_token_index }


let bonding_curve_main (param, storage : bonding_curve_entrypoints * bonding_curve_storage)
    : (operation list) * bonding_curve_storage =
    match param with
    (** admin entrypoints *)
    | Admin admin_param ->
      let ops, admin = admin_main (admin_param, storage.admin) in
      let new_storage = { storage with admin = admin } in
      ops, new_storage

    (** update staking *)
    | Set_delegate delegate_opt ->
      (* ADMIN ONLY *)
      let assert_admin = fail_if_not_admin storage.admin in
      let ops = [Tezos.set_delegate delegate_opt] in
      ops, storage

    (** withdraw unclaimed profits (tracked in storage as 'unclaimed') or fail
        with error_unclaimed_is_zero *)
    | Withdraw withdraw_param ->
      (* ADMIN ONLY *)
      let assert_admin = fail_if_not_admin storage.admin in
      if 0mutez < storage.unclaimed
      then
        let admin : unit contract = resolve_address(storage.admin.admin) in
        let send_op : operation = Tezos.transaction () storage.unclaimed admin in
        let new_storage = { storage with unclaimed = 0mutez } in
        [send_op], new_storage
      else (failwith error_unclaimed_is_zero : (operation list) * bonding_curve_storage)

    (** buy single token on-chain (requires tez deposit)
        see buy_offchain_no_admin *)
    | Buy buy_order_param ->
      buy_offchain_no_admin(Tezos.sender, storage)

    (** buy tokens off-chain (requires all tez deposits)
        I.e. admin buys, but tokens sent -> given address
        see buy_offchain_no_admin *)
    | Buy_offchain offchain_buyer_address ->
      (* ADMIN ONLY *)
      let assert_admin = fail_if_not_admin storage.admin in
      buy_offchain_no_admin(offchain_buyer_address, storage)

    (** sell token on-chain (returns tez deposit)
        see sell_offchain_no_admin *)
    | Sell sell_order_param ->
      sell_offchain_no_admin((sell_order_param, Tezos.sender), storage)

    (** sell single/multi tokens off-chain (returns all tez deposits)
        see sell_offchain_no_admin *)
    | Sell_offchain sell_order_param_offchain_seller_address ->
      (* ADMIN ONLY *)
      let assert_admin = fail_if_not_admin storage.admin in
      sell_offchain_no_admin(sell_order_param_offchain_seller_address, storage)

// Debug-only
#if DEBUG_BONDING_CURVE

     // (n : nat) -> failwith (price in mutez of n-th token w/o basis_points)
     | Cost n ->
       (failwith (run_piecewise_polynomial(storage.cost_mutez, n)) : (operation list) * bonding_curve_storage)

#endif // DEBUG_BONDING_CURVE

