#include "fa2_allowlisted_swap.mligo"
#include "../common.mligo"

type permit_accept_param = 
  [@layout:comb]
  {
    swap_id : nat;
    permit : permit;
  }

type offchain_swap_entry_points =
  | BaseSwap of entrypoints
  | Offchain_accept of permit_accept_param list 

let offchain_accept(param, storage : permit_accept_param list * storage) =
  ([] : operation list), storage

let allowlisted_swaps_offchain_main(param, storage : offchain_swap_entry_points * storage)
    : ((operation list) * storage) = begin
    match param with 
      | BaseSwap entrypoints -> allowlisted_swaps_main(entrypoints, storage)
      | Offchain_accept permits -> offchain_accept(permits, storage)
  end