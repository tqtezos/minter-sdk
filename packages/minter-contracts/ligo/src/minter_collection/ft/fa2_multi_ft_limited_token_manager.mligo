(*
  One of the possible implementations of token management API.
  
  In this implementation, there is only a single new entrypoint added: "Mint". 
  
  "Mint" creates a new token type and mints a fixed amount of it that is distributed to an address. 

  It is not possible to mint more tokens of that type or burn tokens of that type. 

*)

#if !LIMITED_TOKEN_MANAGER
#define LIMITED_TOKEN_MANAGER

#include "fa2_multi_ft_token_manager.mligo"

type mint_fixed_supply_token_param = 
  [@layout:comb]
  {
    owner : address;
    amount : nat;
    token_info : ((string, bytes) map);
  }

type mint_fixed_supply_tokens_param = mint_fixed_supply_token_param list 

(* `token_manager` entry points *)
type token_manager =
  | Mint of mint_fixed_supply_tokens_param

type create_tokens_accumulator = 
  [@layout:comb]
  {
      new_tokens_metadata : token_metadata list;
      mint_tokens_param : mint_burn_tokens_param;
      next_token_id : token_id;
  }

let mint_fixed_supply_tokens (params, storage : 
  mint_fixed_supply_tokens_param * multi_ft_token_storage) : multi_ft_token_storage = 
  let { new_tokens_metadata = new_tokens_metadata; 
        mint_tokens_param = mint_tokens_param;
        next_token_id = next_token_id; 
      } : create_tokens_accumulator = 
    (List.fold 
      (fun (acc, param : create_tokens_accumulator * mint_fixed_supply_token_param) -> 
        let md : token_metadata = {
            token_id = acc.next_token_id;
            token_info = param.token_info;
        }  in 
        let mint_token_param : mint_burn_tx = {
            owner = param.owner;
            amount = param.amount;
            token_id = acc.next_token_id;
        } in 
        let next_token_id : token_id = acc.next_token_id + 1n in 

        ({new_tokens_metadata = md :: acc.new_tokens_metadata;
         mint_tokens_param = mint_token_param :: acc.mint_tokens_param;
         next_token_id = next_token_id;} : create_tokens_accumulator)
      ) 
      params 
      ({
          new_tokens_metadata = ([] : token_metadata list);
          mint_tokens_param = ([] : mint_burn_tokens_param);
          next_token_id = storage.next_token_id;
      } : create_tokens_accumulator)
    ) in 
  let new_s : multi_ft_token_storage = 
    (List.fold_right create_token new_tokens_metadata storage) in 
  let new_s = mint_tokens(mint_tokens_param, new_s) in 
  {new_s with next_token_id = next_token_id}


let ft_token_manager (param, s : token_manager * multi_ft_token_storage)
    : (operation list) * multi_ft_token_storage =
  match param with

    Mint tokens ->
    let new_s = mint_fixed_supply_tokens (tokens, s) in
    ([] : operation list), new_s

#endif
