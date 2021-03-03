
## Notice

This repository is a work in progress. The Minter SDK will contain various functions, such as:
- Contract definitions and tests
- An NPM module that allows users to interact with tokens programmatically
- A CLI that brings SDK functions to the command line

### TODO List

1.  [minter-sdk](#org33105a6)
    1.  [make this agnostic to wallet providers/"signers" (as per Taquito's definition)](#org8958b5a)
    2.  [move all contracts to minter-sdk](#orgb2dc005)
        1.  [auction, fixed price sale, etc.](#org7356af9)
    3.  [put contracts tests in minter-sdk](#orgcb262e5)
    4.  [move bootstrap process outside of sdk (keep in openminter)](#orgb28f4bc)
    5.  [refactor compile-contracts.ts](#org16b8242)
        1.  [streamline the code, make it very basic - ie. compile contracts, put tz in correct places](#orgf0e43ad)
        2.  [maybe use ipfs<sub>hashes</sub>](#org7436438)
    6.  [testing contract origination](#org40d9374)


<a id="org33105a6"></a>

# minter-sdk


<a id="org8958b5a"></a>

## make this agnostic to wallet providers/"signers" (as per Taquito's definition)


<a id="orgb2dc005"></a>

## move all contracts to minter-sdk


<a id="org7356af9"></a>

### auction, fixed price sale, etc.


<a id="orgcb262e5"></a>

## put contracts tests in minter-sdk


<a id="orgb28f4bc"></a>

## move bootstrap process outside of sdk (keep in openminter)


<a id="org16b8242"></a>

## refactor compile-contracts.ts


<a id="orgf0e43ad"></a>

### streamline the code, make it very basic - ie. compile contracts, put tz in correct places


<a id="org7436438"></a>

### maybe use ipfs hashes


<a id="org40d9374"></a>

## testing contract origination
 Here is some code that we should base this test upon
-   (<https://github.com/tqtezos/minter/blob/master/client/src/lib/nfts/actions.ts#L12>)
