#if !OFFCHAIN_SWAP 
#define OFFCHAIN_SWAP

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

let offchain_accept(p, storage, ops : permit_accept_param * storage * operation list) 
  : allowlist_return = begin
    fail_if_not_admin(storage.admin);
    let param_hash = Crypto.blake2b (Bytes.pack p.swap_id) in 
    check_permit (p.permit, 0n, param_hash);
    let swap_accepter = address_from_key (p.permit.signerKey) in
    let swap = get_swap(p.swap_id, storage.swap) in
    let bid_offchain = true in 
    let ops = accept_swap_update_ops_list(swap, swap_accepter, bid_offchain, ops, storage.swap) in
    let swap_storage = accept_swap_update_storage(p.swap_id, swap, swap_accepter, storage.swap) in
    (ops, {storage with swap = swap_storage})
  end

let offchain_accept_batch_helper(ps, (ops, storage) : permit_accept_param list * (operation list * storage)) 
  : allowlist_return = 
    List.fold 
    (fun (((ops_, storage_), permit) : (operation list * storage) * permit_accept_param) -> 
        offchain_accept(permit, storage_, ops_)
    )
    ps 
    (ops, storage)

let offchain_accept_batch(ps, storage : permit_accept_param list * storage) 
  : allowlist_return = begin
    fail_if_not_admin(storage.admin);
    offchain_accept_batch_helper(ps, (([] : operation list), storage))
  end

let allowlisted_swaps_offchain_main(param, storage : offchain_swap_entry_points * storage)
    : allowlist_return = begin
    match param with 
      | BaseSwap entrypoints -> allowlisted_swaps_main(entrypoints, storage)
      | Offchain_accept permits -> offchain_accept_batch(permits, storage)
  end

#endif