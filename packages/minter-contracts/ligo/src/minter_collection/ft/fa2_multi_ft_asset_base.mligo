(* copied from tqtezos/smart-contracts *)
(*
  `multi_asset` contract combines `multi_token` transfer API with
  `admin` API and `token_manager` API.  Input parameter type for the
  `multi_asset` contract is a union of `multi_token` and `admin` parameter
  types.
  Depending on the input, `multi_asset` dispatches call to either
  `multi_token` or `admin`  or `token_manager` entry points.
  If contract is paused, `multi_token` entry points cannot be invoked.
  Only current admin can access `pauseable_admin` and `token_manager` entry points.
*)


type multi_ft_asset_storage = {
  admin : admin_storage;
  assets : multi_ft_token_storage;
  metadata : (string, bytes) big_map; (* contract metadata *)
}

type multi_ft_asset_param =
  | Assets of fa2_entry_points
  | Admin of admin_entrypoints
#if !LIMITED_FT_ASSET
  | Tokens of token_manager
#else
  | [@annot:mint] Tokens of token_manager
#endif

let multi_ft_asset_main
    (param, s : multi_ft_asset_param * multi_ft_asset_storage)
    : (operation list) * multi_ft_asset_storage =
  match param with
  | Admin p ->
      let ops, admin = admin_main (p, s.admin) in
      let new_s = { s with admin = admin; } in
      (ops, new_s)

  | Tokens p ->
      let u1 = fail_if_not_admin s.admin in
      let ops, assets = ft_token_manager (p, s.assets) in
      let new_s = { s with
        assets = assets
      } in
      (ops, new_s)

  | Assets p ->
      let u2 = fail_if_paused s.admin in

      let ops, assets = fa2_main (p, s.assets) in
      let new_s = { s with assets = assets } in
      (ops, new_s)
