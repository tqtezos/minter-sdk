# FA@ Implementation Modules

Reusable modules to use as part of FA2 implementation.

## Admin Modules

Implementation of the admin module with the following public signature:

## Minter Admin Modules

Implementation of the minter admin module with the following signature:

-   `type minter_admin_storage` - minter admin storage
-   `type minter_admin_entrypoints` - minter administration entry points
-   `is_minter (storage : minter_admin_storage) : bool` - a function to guard
    minting in the main contract.

There are following implementations of the minter admin module that share the same
signature, but provide different implementations:

-   `not_minter_admin.mligo` - a default "no op" minter admin. `is_minter` function
    always returns `false`. This admin implementation is useful if the main contract
    wants to implement "admins or minters can mint" or "only admins can mint" semantics
    using the same code, but swapping minter admin implementations:

    ```ocaml
    let fail_if_not_minter(storage : storage) : unit =
    if is_admin(storage.admin)
    then unit (* admin can always mint *)
    else if is_minter(storage.minter_admin)
    then unit (* minter can mint *)
    else failwith "NOT_A_MINTER"
    ```

-   `null_minter_admin.mligo` - minter admin that allows everyone to mint

-   `multi_minter_admin.mligo` - minter that supports set of multiple minters.
    It exposes admin entry points to add or remove minters form the set:

    ```ocaml
    type minter_admin_entrypoints =
      | Add_minter of address
      | Remove_minter of address
    ```
