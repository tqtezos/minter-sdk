#include "common.mligo"

let check_tokens_allowed
    (tokens, allowlist, err : tokens * allowlist * string) : unit =

#if !ALLOWLIST_ENABLED
  unit

#elif ALLOWLIST_SIMPLE
  check_address_allowed(tokens.fa2_address, allowlist, err)

#elif ALLOWLIST_TOKEN
  begin match Big_map.find_opt tokens.fa2_address allowlist with
  | None -> failwith err
  | Some m_tokens_allowlist -> begin match m_tokens_allowlist with
    | All_token_ids_allowed -> unit
    | Token_ids_allowed token_ids_allowlist ->
        List.iter
          (fun (token : fa2_tokens) ->
            if Set.mem token.token_id token_ids_allowlist
              then unit
              else failwith err
          )
          tokens.fa2_batch
    end
  end

#else
<No check_tokens_allowed implementation>
#endif