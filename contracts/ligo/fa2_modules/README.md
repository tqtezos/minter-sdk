# FA2 Implementation Modules

Reusable modules to use as part of FA2 or other contracts implementation.

## Reusable Modules

Each reusable module type has a public module signature consisting of types
and functions and one or more implementations of the same signature. Due to
current limitations of LIGO, public signature is not enforced by the language,
but rather is a naming convention to follow by reusable module developers.

### Admin Modules

Generic contract admin module with the following public signature:

-   `type admin_storage` - admin storage
-   `type admin_entrypoints` - admin administration entry points
-   `fail_if_not_admin (storage : admin_storage) (extra_msg : string option) : unit` -
    an admin guard. Fails if `Tezos.sender` is not an admin.
-   `fail_if_not_admin_ext (storage : admin_storage, extra_msg : string) : unit` -
    an admin guard. Fails if `Tezos.sender` is not an admin. Failure error contains
    an extra message.
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
    pause/unpause the contract, guard other operations to be invoked only by the
    admin and change the admin using two steps confirmation.
    Exposes the following administrative entry points:

    ```ocaml
    type admin_entrypoints =
    | Set_admin of address
    | Confirm_admin of unit
    | Pause of bool
    ```

-   [non_pausable_simple_admin.mligo](admin/non_pausable_simple_admin.mligo) -
    a single admin that can guard other operations to be invoked only by the admin
    and change the admin using two steps confirmation.
    Exposes the following administrative entry points:

    ```ocaml
    type admin_entrypoints =
    | Set_admin of address
    | Confirm_admin of unit
    ```

-   [multi_admin.mligo](admin/multi_admin.mligo) - supports multiple admins for the
    main contract. Admins can pause/unpause the main contract, guard other operations
    to be invoked only by one of the admins and add/remove the admins.
    Exposes the following administrative entry points:

    ```ocaml
    type admin_entrypoints =
    | Set_admin of address
    | Remove_admin of address
    | Confirm_admin of unit
    | Pause of bool
    ```

### Minter Admin Modules

Minter admin module with the following signature:

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

## How to Compose Reusable Modules Into a Single Contract

In general, we want our contract code to depend on some module signature and to
be able to swap different module implementations. The goal is to reuse as much
as possible of the existing code without modifications and to avoid
copy/paste/modify anti-pattern that leads to code duplication. The following
technique is proposed to produce different flavors of the same contract by
"injecting" different implementations of the same module type.

Let's consider an example where we have a module of some type. The module type is
a public signature of the module consisting of some LIGO types and functions.
Module type is not enforced or validated by LIGO, but rather a convention. We have
multiple implementations of the module and a single implementation of the main
contract logic. Main contract logic depends on the module signature (i. e. some
LIGO types and functions provided by the module). Here is how we can organize our
code:

-   Write main contract implementation that depends on some module signature, but
    **does not include any module implementation file**.

    ```ocaml

    type param = ...
    type storage = ...

    let my_main(p, s : param * storage) =
      ...
      let y = module_a_function x in
      ...

    ```

-   Create contract "assembly" file that includes one of the module implementation
    files and the main contract file:

    ```ocaml
    #include "moduleA_implementation1.mligo"
    (* #include "moduleA_implementation2.mligo" *)
    #include "main_contract.mligo"
    ```

-   An "assembly" file includes concrete module implementation before the implementation
    of the main contract and binds module public signature types and functions
    names to the `implementation1`. Now you can compile your contract by executing
    the following command: `ligo compile-contract assembly.mligo my_main`

You can also create different "assemblies" including different module implementation
files without modifying `main_contract.mligo`.

Here is an example that uses **Admin** and **Minter Admin** modules:

-   **Main contract** code [minter_use_example.mligo](examples/minter_use_example.mligo).
    It uses types `admin_storage`, `admin_entrypoints` and functions `is_admin`,
    `fail_if_paused`, `fail_if_not_admin` from `admin` module and types
    `minter_admin_storage`, `minter_admin_entrypoints` and a function `is_minter`
    from `minter_admin` module. Different module implementations can be used.
-   **Assembly** [minter_use_example_simple_admin_with_minters.mligo](examples/minter_use_example_simple_admin_with_minters.mligo).
    The assembly "injects" `simple_admin` and `multi_minter_admin` module implementations
    respectively.
