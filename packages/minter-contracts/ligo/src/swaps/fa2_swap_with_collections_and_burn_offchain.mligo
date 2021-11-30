#if !OFFCHAIN_SWAP 
#define OFFCHAIN_SWAP

#include "fa2_swap_with_collections_and_burn.mligo"
#include "../common.mligo"

type permit_accept_param = 
  [@layout:comb]
  {
    accept_param : accept_param;
    permit : permit;
  }

type offchain_swap_entry_points =
  | BaseSwap of swap_entrypoints
  | Offchain_accept of permit_accept_param list

let offchain_accept(p, storage, ops : permit_accept_param * swap_storage * operation list) 
  : return = begin
    fail_if_not_admin(storage.admin);
    let param_hash = Crypto.blake2b (Bytes.pack p.accept_param) in 
    check_permit (p.permit, 0n, param_hash);
    let swap_accepter = address_from_key (p.permit.signerKey) in
    let swap = get_swap(p.accept_param.swap_id, storage) in
    let bid_offchain = true in 
    let ops = accept_swap_update_ops_list(swap, p.accept_param.tokens, swap_accepter, bid_offchain, ops, storage) in
    let new_storage = accept_swap_update_storage(p.accept_param.swap_id, swap, swap_accepter, storage) in
    (ops, new_storage)
  end

let offchain_accept_batch_helper(ps, (ops, storage) : permit_accept_param list * (operation list * swap_storage)) 
  : return = 
    List.fold 
    (fun (((ops_, storage_), permit) : (operation list * swap_storage) * permit_accept_param) -> 
        offchain_accept(permit, storage_, ops_)
    )
    ps 
    (ops, storage)

let offchain_accept_batch(ps, storage : permit_accept_param list * swap_storage) 
  : return = begin
    fail_if_not_admin(storage.admin);
    offchain_accept_batch_helper(ps, (([] : operation list), storage))
  end

let swaps_offchain_main(param, storage : offchain_swap_entry_points * swap_storage)
    : return = begin
    match param with 
      | BaseSwap entrypoints -> swaps_main(entrypoints, storage)
      | Offchain_accept permits -> offchain_accept_batch(permits, storage)
  end

#endif
