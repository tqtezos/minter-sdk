(* Simple allowlist of addresses.
 *
 * It is aimed at use cases with very small allowlists (just few addresses).
*)

#if !ALLOWLIST_SIMPLE
#define ALLOWLIST_SIMPLE
#define ALLOWLIST_ENABLED

#include "../../fa2/fa2_interface.mligo"

type allowlist = (address, unit) big_map

type allowlist_entrypoints = allowlist

let init_allowlist : allowlist = []

let update_allowed (param, allowlist : allowlist_entrypoints * allowlist) : allowlist =
  param

let check_address_allowed (addr, allowlist, err : address * allowlist * string) : unit =
  match Big_map.find_opt addr allowlist with
  | Some u -> u
  | None -> failwith err

let check_single_token_allowed
    (addr, token_id, allowlist, err : address * token_id * allowlist * string) : unit =
  check_address_allowed(addr, allowlist, err)

#endif
