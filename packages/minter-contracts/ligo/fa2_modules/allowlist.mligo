#if !ALLOWLIST
#define ALLOWLIST

type allowlist = (address, unit) big_map

let check_address_allowed (addr, allowlist, err : address * allowlist * string) : unit =
  match Big_map.find_opt addr allowlist with
  | Some u -> u
  | None -> failwith err

#endif
