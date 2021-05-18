# Multi-Asset FA2 Contract for Non-Fungible Tokens

This directory contains various flavors of a Multi-Asset Non-Fungible Token variant of the [FA2 Token Standard](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md). The contracts come with various configurations for Admin roles with respect to minting new tokens and pausing/unpausing the contract.  

 The multi-asset contract manages multiple token types. A new token can be created and configured without redeploying the contract. This design enables new functionality, such as the transfer of multiple token types at once, saving on transaction costs. 

 The code is written with a high degree of modularity with seperate components seperated into different LIGO files to ease future extension. This document will focous on these different components. 

![Mermaid NFT Code Flowchart](../../../../../../docs/mermaid-diagram-nft.png)

 ## [FA2 Multi NFT Token](fa2_multi_nft_token.mligo) (TOKEN)

The code in this file manages the aspects of the contract described by the [FA2 Token Standard](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md). These include the entrypoints `transfer`, `balance_of`, `update_operators`.  Refer to this standard for more details. Note that `Mint_tokens` is not included in this file as TZIP-12 does not provide a specification for that entrypoint. 

It also contains the optional implementation of a more complex FA2 transfer policy as per the specification provided in [TZIP-12](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/permissions-policy.md). This functionality can be enabled by defining the C Macro OWNER_HOOKS in contracts that extend TOKEN. 

## [FA2 Multi NFT Faucet](fa2_multi_nft_faucet.mligo) (FAUCET)

FAUCET simply combines the functionality provided by TOKEN with the ability to mint an NFT. Unlike the [Fungible-Token FA2 Faucet](../ft/fa2_multi_ft_faucet.mligo), it is only possible to mint a single token for a given `token-id` (hence the name Non-Fungible). FAUCET does not provide any admin-capabilities so it is impossible to set a new admin or pause the contract. Additionally, anyone can mint new NFTs using this contract as there is no admin check. 

Minting N tokens in batch is accomplished through a call to the entrypoint

```
Mint : (list %mint
           (pair (pair %token_metadata (nat %token_id) (map %token_info string bytes))
                 (address %owner)))
```

and passing a list of length N. 

Whereas "creating" and "minting" is a two step process in the FT contracts, it is accomplished in a single entrypoint call in this contract. Also, it is not possible to "burn" an NFT at the moment using this contract, whereas that can be accomplished with the FT contract. 

## [FA2 Multi NFT Asset](fa2_multi_nft_asset.mligo) (ASSET)

ASSET, like FAUCET, extends TOKEN to add a `Mint` entrypoint. It also adds an interface for providing admin capabilites to the contract. In contrast with the faucet implementation, only the admin can mint tokens. "Admin capabilities" means the type of functionality provided by the [Admin modules](../../../fa2_modules/README.md): possibly including but not limited to the ability to add new admins, remove admins, pause the contract, and confirm an added admin (in a two-step approval pattern). ASSET will not compile on its own, but will need to be implemented by specifying which admin module ought to apply for the contract. [SIMPLE_ADMIN](fa2_multi_nft_asset_simple_admin.mligo), [MULTI_ADMIN](fa2_multi_nft_asset_multi_admin.mligo), [NO_ADMIN](fa2_multi_nft_asset_no_admin.mligo), and [NON_PAUSABLE_SIMPLE_ADMIN](fa2_multi_nft_asset_non_pausable_simple_admin.mligo) are simply ASSET implemented with different admin modules of the same name. Their respective functionalities can be read about in [Admin modules README](../../../fa2_modules/README.md). 

## EDITIONS Version of NFT contracts

When the EDITIONS C-Macro is defined as in the [Editions FA2 Contract](../editions/fa2_multi_nft_token_editions.mligo), the default NFT FA2 contract is changed slightly. The changes are listed below: 
 - The `token_metadata` big_map is removed.
 - The `next_token_id` storage variable is removed. 
 - The standard `Mint` entrypoint is removed. 
 - Adds `mint_edition_set` to `fa2_multi_nft_manager.mligo`, which functions similarly to `mint_tokens` defined in that file without updating `token_metadata` big_map which is deleted for editions. 
