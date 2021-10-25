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
  | Offchain_accept of permit_accept_param

let offchain_accept(p, storage : permit_accept_param * storage) = begin
    fail_if_not_admin(storage.admin);
    let param_hash = Crypto.blake2b (Bytes.pack p.swap_id) in 
    check_permit (p.permit, 0n, param_hash);
    let swap_accepter = address_from_key (p.permit.signerKey) in
    let (ops, swap_storage) = accept_swap(p.swap_id, swap_accepter, storage.swap) in 
    (ops, {storage with swap = swap_storage})
  end

let allowlisted_swaps_offchain_main(param, storage : offchain_swap_entry_points * storage)
    : ((operation list) * storage) = begin
    match param with 
      | BaseSwap entrypoints -> allowlisted_swaps_main(entrypoints, storage)
      | Offchain_accept permit -> offchain_accept(permit, storage)
  end