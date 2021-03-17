# FA@ Implementation Modules

Reusable modules to use as part of FA2 implementation.

## Admin Modules

Implementation of the admin module with the following public signature:

-   `type admin_storage` - admin storage
-   `type admin_entrypoints` - admin administration entry points
-   `fail_if_not_admin (storage : admin_storage) (extra_msg : string option) : unit` -
    an admin guard. Fails if `Tezos.sender is not an admin.
-   `is_admin (storage : admin_storage) : bool` - an admin guard.
    Returns `true` if `Tezos.sender` is an admin.
-   `fail_if_paused (storage : admin_storage) : unit` - pause guard. Fails if the
    contract is in paused state
-   `admin_main(param, storage : admin_entrypoints * admin_storage) : (operation list) * admin_storage` -
    a function to handle admin entry points if any

There are following implementations of the admin module that share the same
signature:

-   [no_admin.mligo](admin/no_admin.mligo) - everyone is admin
-   [simple_admin.mligo](admin/simple_admin.mligo) - a single admin that can
    pause/unpause the contract and change the admin using two steps confirmation.
    Exposes the following administrative entry points:

    ```ocaml
    type admin_entrypoints =
    | Set_admin of address
    | Confirm_admin of unit
    | Pause of bool
    ```

-   [multi_admin.mligo](admin/multi_admin.mligo) - supports multiple admins for the
    main contract. Admins can pause/unpause the main contract and add/remove the
    admins. Exposes the following administrative entry points:

    ```ocaml
    type admin_entrypoints =
    | Set_admin of address
    | Remove_admin of address
    | Confirm_admin of unit
    | Pause of bool
    ```

## Minter Admin Modules

Implementation of the minter admin module with the following signature:

-   `type minter_admin_storage` - minter admin storage
-   `type minter_admin_entrypoints` - minter administration entry points
-   `is_minter (storage : minter_admin_storage) : bool` - a minter guard.
    Returns `true` if `Tezos.sender` is a minter.
-   `minter_admin_main(param, storage : minter_admin_entrypoints * minter_admin_storage)` -
    a function to handle admin entry points if any.

There are following implementations of the minter admin module that share the same
signature:

-   [not_minter_admin.mligo](minter_admin/not_minter_admin.mligo) - a default
    "no op" minter admin. `is_minter` function always returns `false`. This admin
    implementation is useful if the main contract wants to implement "admins or
    minters can mint" or "only admins can mint" semantics using the same code,
    but swapping minter admin implementations:

    ```ocaml
    let fail_if_not_minter(storage : storage) : unit =
    if is_admin(storage.admin)
    then unit (* admin can always mint *)
    else if is_minter(storage.minter_admin)
    then unit (* minter can mint *)
    else failwith "NOT_A_MINTER"
    ```

-   [null_minter_admin.mligo](minter_admin/null_minter_admin.mligo) - minter admin
    that allows everyone to mint

-   [multi_minter_admin.mligo](minter_admin/multi_minter_admin.mligo) - minter admin
    that supports set of multiple minters. It exposes administration entry points
    to add or remove minters form the set:

    ```ocaml
    type minter_admin_entrypoints =
      | Add_minter of address
      | Remove_minter of address
    ```
