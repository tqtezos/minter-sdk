(* Base allowlisting module.
 * Import it in a contract if it is planned to be extended with allowlisting
 * extensions.
 * This module adds no allowlisting (i.e. allowlist is empty and cannot be updated)
 * unless some other allowlisting module is imported beforehand.
*)

#if !ALLOWLIST_BASE
#define ALLOWLIST_BASE

#if !ALLOWLIST_ENABLED

type allowlist = unit

type allowlist_entrypoints = never

let init_allowlist : allowlist = unit

let check_single_token_allowed
    (addr, token_id, allowlist, err : address * token_id * allowlist * string) : unit =
  unit

#endif

#endif !ALLOWLIST_BASE
