(* Simple allowlist of addresses *)

#if !ALLOWLIST_SIMPLE
#define ALLOWLIST_SIMPLE
#define ALLOWLIST_ENABLED

type allowlist = (address, unit) big_map

type allowlist_entrypoints = allowlist

let init_allowlist : allowlist = []

let check_address_allowed (addr, allowlist, err : address * allowlist * string) : unit =
  match Big_map.find_opt addr allowlist with
  | Some u -> u
  | None -> failwith err

#endif
