(* Allowlist of FA2 addresses and token_ids.
 *
 * This allows permitting only specific addresses, and for some addresses
 * also restricting the set of allowed token_ids.
 * This suits for large addresses sets, however token_ids set per each address
 * is assumed to be small.
*)

#if !ALLOWLIST_TOKEN
#define ALLOWLIST_TOKEN
#define ALLOWLIST_ENABLED

#include "../../fa2/fa2_interface.mligo"

type allowed_token_ids =
  | All_token_ids_allowed
  | Token_ids_allowed of token_id set

type allowlist = (address, allowed_token_ids) big_map

type allowlist_update =
  [@layout:comb]
  { to_remove : address set
  ; to_add : (address, allowed_token_ids) map
  }

type allowlist_entrypoints = allowlist_update

let init_allowlist : allowlist = []

let update_allowed (param, allowlist : allowlist_entrypoints * allowlist) : allowlist =
  let allowlist =
        Set.fold
          (fun (allowlist, addr : allowlist * address) ->
            Big_map.remove addr allowlist
          )
          param.to_remove allowlist
      in
  let allowlist =
        Map.fold
          (fun (allowlist, (addr, allowed_token_ids) : allowlist * (address * allowed_token_ids)) ->
            Big_map.add addr allowed_token_ids allowlist
          )
          param.to_add allowlist
    in
  allowlist

let check_single_token_allowed
    (addr, token_id, allowlist, err : address * token_id * allowlist * string) : unit =
  begin match Big_map.find_opt addr allowlist with
  | None -> failwith err
  | Some allowed_token_ids -> begin match allowed_token_ids with
    | All_token_ids_allowed -> unit
    | Token_ids_allowed token_ids_allowlist ->
        if Set.mem token_id token_ids_allowlist
          then unit
          else failwith err
    end
  end


#endif
