# Dutch style NFT Auction with tickets

This project implements a dutch style auction, using NFTs represented by tickets instead of traditional FA2 based NFTs.

# Demo

The two nft-wallet contracts (compiled using an older ligo version so possibly with slightly different Michelson) as well as Alice's nft-auction contract were added to edonet and can be viewed on tzstats.

* Alice's wallet: https://edo.tzstats.com/KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea
* Bob's wallet: https://edo.tzstats.com/KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs
* Alice's nft-auction: https://edo.tzstats.com/KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG

## 1. Alice originates nft-auction contract
```
$ tezos-client originate contract nft-auction transferring 0 from alice running "$(pwd)/michelson/nft_auction.tz" --dry-run --init "Pair (Pair \"tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt\" (Pair 0 (Pair 0 (Pair False ( Pair 0 0))))) {}" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BLjqDeA5khok (timestamp: 2021-01-06T00:27:58.000-00:00, validation: 2021-01-06T00:28:15.004-00:00)
Node is bootstrapped.
Estimated gas: 12295.862 units (will add 100 for safety)
Estimated storage: 2534 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oofgtQf3UjQPnZ67LV9erMR7ijhKs3EZPJmckEWeec746TAUDRq'
Waiting for the operation to be included...
Operation found in block: BL4Dgpq8B8idYiCBRBgj41Ck3LXqR1CHCkhJujDcWWjAvexuXXY (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.003734
    Expected counter: 103191
    Gas limit: 12396
    Storage limit: 2554 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.003734
      fees(tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9,50) ... +ꜩ0.003734
    Origination:
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      Credit: ꜩ0
      Script:
        { parameter
            (or (or (or (contract %buy (ticket nat)) (contract %cancel (ticket nat)))
                    (or (pair %configure
                           (nat %opening_price)
                           (pair (nat %set_reserve_price)
                                 (pair (timestamp %set_start_time) (pair (int %set_round_time) (ticket %ticket nat)))))
                        (nat %drop_price)))
                (unit %start)) ;
          storage
            (pair (pair %data
                     (address %admin)
                     (pair (nat %current_price)
                           (pair (nat %reserve_price)
                                 (pair (bool %in_progress) (pair (timestamp %start_time) (int %round_time))))))
                  (big_map %tickets nat (ticket nat))) ;
          code { UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 DIG 2 ;
                 IF_LEFT
                   { IF_LEFT
                       { IF_LEFT
                           { NOW ;
                             AMOUNT ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CAR ;
                             SENDER ;
                             COMPARE ;
                             NEQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             PUSH mutez 1 ;
                             DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             CDR ;
                             CAR ;
                             MUL ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             ADD ;
                             DIG 2 ;
                             COMPARE ;
                             LE ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CAR ;
                             CONTRACT unit ;
                             IF_NONE
                               { DROP 4 ; PUSH string "contract does not match" ; FAILWITH }
                               { SWAP ;
                                 PUSH unit Unit ;
                                 TRANSFER_TOKENS ;
                                 DIG 3 ;
                                 NONE (ticket nat) ;
                                 PUSH nat 0 ;
                                 GET_AND_UPDATE ;
                                 IF_NONE
                                   { DROP 4 ; PUSH string "ticket does not exist" ; FAILWITH }
                                   { DIG 3 ;
                                     PUSH mutez 0 ;
                                     DIG 2 ;
                                     TRANSFER_TOKENS ;
                                     SWAP ;
                                     DIG 3 ;
                                     DUP ;
                                     DUG 4 ;
                                     CDR ;
                                     CDR ;
                                     CDR ;
                                     CDR ;
                                     PUSH bool False ;
                                     PAIR ;
                                     DIG 4 ;
                                     DUP ;
                                     DUG 5 ;
                                     CDR ;
                                     CDR ;
                                     CAR ;
                                     PAIR ;
                                     DIG 4 ;
                                     DUP ;
                                     DUG 5 ;
                                     CDR ;
                                     CAR ;
                                     PAIR ;
                                     DIG 4 ;
                                     CAR ;
                                     PAIR ;
                                     PAIR ;
                                     NIL operation ;
                                     DIG 2 ;
                                     CONS ;
                                     DIG 2 ;
                                     CONS ;
                                     PAIR } } }
                           { SWAP ;
                             DUP ;
                             DUG 2 ;
                             CAR ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             NONE (ticket nat) ;
                             PUSH nat 0 ;
                             GET_AND_UPDATE ;
                             IF_NONE
                               { DROP 3 ; PUSH string "ticket does not exist" ; FAILWITH }
                               { DIG 2 ;
                                 PUSH mutez 0 ;
                                 DIG 2 ;
                                 TRANSFER_TOKENS ;
                                 SWAP ;
                                 DIG 2 ;
                                 DUP ;
                                 DUG 3 ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 PUSH bool False ;
                                 PAIR ;
                                 DIG 3 ;
                                 DUP ;
                                 DUG 4 ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 3 ;
                                 DUP ;
                                 DUG 4 ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 3 ;
                                 CAR ;
                                 PAIR ;
                                 PAIR ;
                                 NIL operation ;
                                 DIG 2 ;
                                 CONS ;
                                 PAIR } } }
                       { IF_LEFT
                           { SWAP ;
                             DUP ;
                             DUG 2 ;
                             CAR ;
                             SOURCE ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             NOT ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             UNPAIR ;
                             SWAP ;
                             UNPAIR ;
                             SWAP ;
                             UNPAIR ;
                             SWAP ;
                             UNPAIR ;
                             DIG 6 ;
                             DIG 2 ;
                             SOME ;
                             PUSH nat 0 ;
                             UPDATE ;
                             SWAP ;
                             DIG 2 ;
                             PAIR ;
                             PUSH bool False ;
                             PAIR ;
                             DIG 2 ;
                             PAIR ;
                             DIG 2 ;
                             PAIR ;
                             DIG 2 ;
                             CAR ;
                             PAIR ;
                             PAIR ;
                             NIL operation ;
                             PAIR }
                           { NOW ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CAR ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CDR ;
                             CDR ;
                             CAR ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             COMPARE ;
                             GE ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             ADD ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             COMPARE ;
                             GT ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CDR ;
                             CDR ;
                             DIG 3 ;
                             PAIR ;
                             DIG 3 ;
                             CAR ;
                             PAIR ;
                             DUP ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             DIG 3 ;
                             PAIR ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CAR ;
                             PAIR ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CDR ;
                             CAR ;
                             PAIR ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CAR ;
                             PAIR ;
                             SWAP ;
                             CAR ;
                             PAIR ;
                             PAIR ;
                             NIL operation ;
                             PAIR } } }
                   { DROP ;
                     NOW ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     SOURCE ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     NOT ;
                     IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     COMPARE ;
                     GE ;
                     IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                     DIG 2 ;
                     NONE (ticket nat) ;
                     PUSH nat 0 ;
                     GET_AND_UPDATE ;
                     IF_NONE
                       { DROP 3 ; PUSH string "no ticket" ; FAILWITH }
                       { SOME ;
                         PUSH nat 0 ;
                         UPDATE ;
                         DIG 2 ;
                         DUP ;
                         DUG 3 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CDR ;
                         PUSH bool True ;
                         PAIR ;
                         DIG 3 ;
                         DUP ;
                         DUG 4 ;
                         CDR ;
                         CDR ;
                         CAR ;
                         PAIR ;
                         DIG 3 ;
                         DUP ;
                         DUG 4 ;
                         CDR ;
                         CAR ;
                         PAIR ;
                         DIG 3 ;
                         CAR ;
                         PAIR ;
                         DUP ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CDR ;
                         DIG 3 ;
                         PAIR ;
                         SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         PAIR ;
                         SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CAR ;
                         PAIR ;
                         SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CAR ;
                         PAIR ;
                         SWAP ;
                         CAR ;
                         PAIR ;
                         PAIR ;
                         NIL operation ;
                         PAIR } } } }
        Initial storage:
          (Pair (Pair "tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt" (Pair 0 (Pair 0 (Pair False (Pair 0 0)))))
                {})
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
        Storage size: 2277 bytes
        Updated big_maps:
          New map(139) of type (big_map nat (ticket nat))
        Paid storage size diff: 2277 bytes
        Consumed gas: 12295.862
        Balance updates:
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.56925
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.06425

New contract KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG originated.
The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for oofgtQf3UjQPnZ67LV9erMR7ijhKs3EZPJmckEWeec746TAUDRq to be included --confirmations 30 --branch BLjqDeA5khokn71SHexRDVESL8tQrdZdKqK2ZdDQo5M7Z7fGMX4
and/or an external block explorer.
Contract memorized as nft-auction.
```

## 2. Alice originates her nft-wallet contract and sets herself as the admin
```
$ tezos-client originate contract nft-wallet transferring 0 from alice running "$(pwd)/michelson/nft_wallet.tz" --init "Pair \"tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt\" (Pair {} (Pair 0 {}))" --burn-cap 0.305

Waiting for the node to be bootstrapped...
Current head: BMFHdefZKb5g (timestamp: 2021-01-06T01:26:38.000-00:00, validation: 2021-01-06T01:27:00.749-00:00)
Node is bootstrapped.
Estimated gas: 7763.475 units (will add 100 for safety)
Estimated storage: 1725 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opEQ4YGA2W4HNK3aT7KEwRaWwUYUwYX96Vx1A57PxtqsUvvDCeR'
Waiting for the operation to be included...
Operation found in block: BMGXejtwcaBnVhVyVPngrRszStasV3uzpGpwTq358rrSwJdCBfx (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.002437
    Expected counter: 103193
    Gas limit: 7864
    Storage limit: 1745 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.002437
      fees(tz1VWasoyFGAWZt5K2qZRzP3cWzv3z7MMhP8,50) ... +ꜩ0.002437
    Origination:
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      Credit: ꜩ0
      Script:
        { parameter
            (or (or (or (pair %auction
                           (contract %destination
                              (pair (nat %opening_price)
                                    (pair (nat %set_reserve_price)
                                          (pair (timestamp %set_start_time) (pair (int %set_round_time) (ticket %ticket nat))))))
                           (pair (nat %opening_price)
                                 (pair (nat %reserve_price)
                                       (pair (timestamp %start_time) (pair (int %round_time) (nat %ticket_id))))))
                        (nat %burn))
                    (or (map %mint string bytes) (ticket %receive nat)))
                (pair %send (contract %destination (ticket nat)) (nat %ticket_id))) ;
          storage
            (pair (address %admin)
                  (pair (big_map %tickets nat (ticket nat))
                        (pair (nat %current_id) (big_map %token_metadata nat (pair nat (map string bytes)))))) ;
          code { PUSH mutez 0 ;
                 AMOUNT ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 DIG 4 ;
                 IF_LEFT
                   { IF_LEFT
                       { IF_LEFT
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             NONE (ticket nat) ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             GET_AND_UPDATE ;
                             IF_NONE
                               { DROP 5 ; PUSH string "no tickets" ; FAILWITH }
                               { DIG 2 ;
                                 DUP ;
                                 DUG 3 ;
                                 CAR ;
                                 PUSH mutez 0 ;
                                 DIG 2 ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 TRANSFER_TOKENS ;
                                 DIG 3 ;
                                 DIG 3 ;
                                 PAIR ;
                                 DIG 2 ;
                                 PAIR ;
                                 DIG 2 ;
                                 PAIR ;
                                 NIL operation ;
                                 DIG 2 ;
                                 CONS ;
                                 PAIR } }
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             DIG 2 ;
                             NONE (ticket nat) ;
                             DIG 3 ;
                             UPDATE ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR } }
                       { IF_LEFT
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             TICKET ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             GET_AND_UPDATE ;
                             DROP ;
                             DIG 3 ;
                             DIG 2 ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             PAIR ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             UPDATE ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR }
                           { READ_TICKET ;
                             UNPAIR ;
                             DROP ;
                             UNPAIR ;
                             DROP ;
                             DIG 4 ;
                             DIG 2 ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             GET_AND_UPDATE ;
                             DROP ;
                             PUSH nat 1 ;
                             DIG 2 ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR } } }
                   { DIG 4 ;
                     DUP ;
                     DUG 5 ;
                     SENDER ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                     DIG 3 ;
                     NONE (ticket nat) ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     CDR ;
                     GET_AND_UPDATE ;
                     IF_NONE
                       { DROP 5 ; PUSH string "no tickets" ; FAILWITH }
                       { DIG 2 ;
                         CAR ;
                         PUSH mutez 0 ;
                         DIG 2 ;
                         TRANSFER_TOKENS ;
                         DIG 3 ;
                         DIG 3 ;
                         PAIR ;
                         DIG 2 ;
                         PAIR ;
                         DIG 2 ;
                         PAIR ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } } } }
        Initial storage:
          (Pair "tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt" (Pair {} (Pair 0 {})))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea
        Storage size: 1468 bytes
        Updated big_maps:
          New map(141) of type (big_map nat (pair nat (map string bytes)))
          New map(140) of type (big_map nat (ticket nat))
        Paid storage size diff: 1468 bytes
        Consumed gas: 7763.475
        Balance updates:
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.367
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.06425

New contract KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea originated.
The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opEQ4YGA2W4HNK3aT7KEwRaWwUYUwYX96Vx1A57PxtqsUvvDCeR to be included --confirmations 30 --branch BMFHdefZKb5gWUoDW9gEJ7Z2Y5NZgFthsGKzZCq9VNa8nV7xyhW
and/or an external block explorer.
Contract memorized as nft-wallet.
```
## 3. Alice mints herself a ticket based nft with metadata
```
$ tezos-client transfer 0 from alice to nft-wallet --entrypoint "mint" --arg "{Elt \"\" 0x68747470733a2f2f6769746875622e636f6d2f747174657a6f732f7469636b65742d7475746f7269616c732f747265652f6d61696e2f7475746f7269616c732f61756374696f6e2f6c69676f}" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BMYZUPD6fiFC (timestamp: 2021-01-06T01:27:38.000-00:00, validation: 2021-01-06T01:27:56.209-00:00)
Node is bootstrapped.
Estimated gas: 9679.058 units (will add 100 for safety)
Estimated storage: 262 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opZoMEUqAQPRysiY9dKLduwwdurQNRFufSNSDWuquEiYgdHzsQd'
Waiting for the operation to be included...
Operation found in block: BL1zqiPMAk6x5ezZ5XZ7QrWAsSW4XUr57mbNgPkjK8SKBZq1ZTJ (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.001332
    Expected counter: 103194
    Gas limit: 9780
    Storage limit: 282 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.001332
      fees(tz1Na5QB98cDA3BC1SQU4w3iiWGVGktU14LE,50) ... +ꜩ0.001332
    Transaction:
      Amount: ꜩ0
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      To: KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea
      Entrypoint: mint
      Parameter: { Elt ""
                       0x68747470733a2f2f6769746875622e636f6d2f747174657a6f732f7469636b65742d7475746f7269616c732f747265652f6d61696e2f7475746f7269616c732f61756374696f6e2f6c69676f }
      This transaction was successfully applied
      Updated storage:
        { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ; 140 ; 1 ; 141 }
      Updated big_maps:
        Set map(141)[0] to (Pair 0
              { Elt ""
                    0x68747470733a2f2f6769746875622e636f6d2f747174657a6f732f7469636b65742d7475746f7269616c732f747265652f6d61696e2f7475746f7269616c732f61756374696f6e2f6c69676f })
        Set map(140)[0] to (Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1))
      Storage size: 1730 bytes
      Paid storage size diff: 262 bytes
      Consumed gas: 9679.058
      Balance updates:
        tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.0655

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opZoMEUqAQPRysiY9dKLduwwdurQNRFufSNSDWuquEiYgdHzsQd to be included --confirmations 30 --branch BMYZUPD6fiFCZ2rpncMoCb9DrAdKCVdyHc8HNFJYwnFtFcdk4Vy
and/or an external block explorer.
```
## 4. Alice auctions off her ticket based nft through her wallet, which sends her nft to her auction contract and configures various auction settings. The starting price of the auction is 100 mutez.

```
$ tezos-client transfer 0 from alice to nft-wallet --entrypoint "auction" --arg "Pair \"KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG%configure\" (Pair 100 (Pair 10 (Pair 0 (Pair 600 0))))" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BLUeXUpmoSFg (timestamp: 2021-01-06T01:29:38.000-00:00, validation: 2021-01-06T01:30:07.976-00:00)
Node is bootstrapped.
Estimated gas: 34374.951 units (will add 100 for safety)
Estimated storage: 102 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooK5QXMJBWXPvEZDzaMs89D9UYFYfYmQNoaKuB4zgqCj1Tei8ci'
Waiting for the operation to be included...
Operation found in block: BKmS5cCwUGtDkrv4iEqS1YJXo9WLSsxYACfzmmpPTJrDQusTxE2 (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.003785
    Expected counter: 103195
    Gas limit: 34475
    Storage limit: 122 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.003785
      fees(tz1Na5QB98cDA3BC1SQU4w3iiWGVGktU14LE,50) ... +ꜩ0.003785
    Transaction:
      Amount: ꜩ0
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      To: KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea
      Entrypoint: auction
      Parameter: (Pair "KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG%configure"
                       (Pair 100 (Pair 10 (Pair 0 (Pair 600 0)))))
      This transaction was successfully applied
      Updated storage:
        { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ; 140 ; 1 ; 141 }
      Updated big_maps:
        Unset map(140)[0]
      Storage size: 1630 bytes
      Consumed gas: 20672.886
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1EAMUQC1yJ2sRPNPpLHVMGCzroYGe1C1ea
        To: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
        Entrypoint: configure
        Parameter: { 100 ;
                     10 ;
                     0 ;
                     600 ;
                     Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1) }
        This transaction was successfully applied
        Updated storage:
          (Pair { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ;
                  100 ;
                  10 ;
                  False ;
                  0 ;
                  600 }
                139)
        Updated big_maps:
          Set map(139)[0] to (Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1))
        Storage size: 2379 bytes
        Paid storage size diff: 102 bytes
        Consumed gas: 13702.065
        Balance updates:
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.0255

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for ooK5QXMJBWXPvEZDzaMs89D9UYFYfYmQNoaKuB4zgqCj1Tei8ci to be included --confirmations 30 --branch BM82CX9HwyUfhxLT4zUmhQj3KhUKhNCeGvcAmnHfgCiG8XHhYEd
and/or an external block explorer.
```
## 5. Alice starts her nft-auction by calling the nft-auction contract directly

```
$ tezos-client transfer 0 from alice to nft-auction --entrypoint "start" --burn-cap 1  

Waiting for the node to be bootstrapped...
Current head: BLZh6tEhMq6n (timestamp: 2021-01-06T01:38:08.000-00:00, validation: 2021-01-06T01:38:27.055-00:00)
Node is bootstrapped.
Estimated gas: 14131.842 units (will add 100 for safety)
Estimated storage: 4 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opMf6wPTpaWmufemQnGyvbM7onTifqB296UffHokBwrxDaSXomj'
Waiting for the operation to be included...
Operation found in block: BLts9co3ToRazepdtna9dUXUNKEAR73ZZK6FfPmgyAqH1TnNy3k (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.001687
    Expected counter: 103196
    Gas limit: 14232
    Storage limit: 24 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.001687
      fees(tz1VWasoyFGAWZt5K2qZRzP3cWzv3z7MMhP8,50) ... +ꜩ0.001687
    Transaction:
      Amount: ꜩ0
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      To: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
      Entrypoint: start
      This transaction was successfully applied
      Updated storage:
        (Pair { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ;
                100 ;
                10 ;
                True ;
                1609897118 ;
                600 }
              139)
      Updated big_maps:
        Set map(139)[0] to (Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1))
      Storage size: 2383 bytes
      Paid storage size diff: 4 bytes
      Consumed gas: 14131.842
      Balance updates:
        tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... -ꜩ0.001

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opMf6wPTpaWmufemQnGyvbM7onTifqB296UffHokBwrxDaSXomj to be included --confirmations 30 --branch BLZh6tEhMq6nwcNvZHsduc3jeA99MdUmWCAeMrjAz66NWTStihL
and/or an external block explorer.
```
## 6. Bob originates his nft wallet and sets himself as the admin
```
tezos-client originate contract nft-wallet-bob transferring 0 from bob running "$(pwd)/michelson/nft_wallet.tz" --init "Pair \"tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y\" (Pair {} (Pair 0 {}))" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BL6uqjE8sD1y (timestamp: 2021-01-06T01:49:38.000-00:00, validation: 2021-01-06T01:49:43.592-00:00)
Node is bootstrapped.
Estimated gas: 7763.475 units (will add 100 for safety)
Estimated storage: 1725 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opTMzFwbY9HvmQpTff5ViujAFVqguz5TwZE9FoKWmGT9onsnCmq'
Waiting for the operation to be included...
Operation found in block: BLjD53iKVbm8eA166a7Py8bLN1SGqA7xhsfk1Gfmx92dk1gq8Br (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
    Fee to the baker: ꜩ0.000359
    Expected counter: 103189
    Gas limit: 1000
    Storage limit: 0 bytes
    Balance updates:
      tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ............ -ꜩ0.000359
      fees(tz1dAfFc4QAre74yrPU2jFBLcgaAs9MLHryD,50) ... +ꜩ0.000359
    Revelation of manager public key:
      Contract: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
      Key: edpkuj3qiKfzbptop91V44ia1NBomcQKgfgDk6mhT9EB73SEmgY6N7
      This revelation was successfully applied
      Consumed gas: 1000
  Manager signed operations:
    From: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
    Fee to the baker: ꜩ0.002341
    Expected counter: 103190
    Gas limit: 7864
    Storage limit: 1745 bytes
    Balance updates:
      tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ............ -ꜩ0.002341
      fees(tz1dAfFc4QAre74yrPU2jFBLcgaAs9MLHryD,50) ... +ꜩ0.002341
    Origination:
      From: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
      Credit: ꜩ0
      Script:
        { parameter
            (or (or (or (pair %auction
                           (contract %destination
                              (pair (nat %opening_price)
                                    (pair (nat %set_reserve_price)
                                          (pair (timestamp %set_start_time) (pair (int %set_round_time) (ticket %ticket nat))))))
                           (pair (nat %opening_price)
                                 (pair (nat %reserve_price)
                                       (pair (timestamp %start_time) (pair (int %round_time) (nat %ticket_id))))))
                        (nat %burn))
                    (or (map %mint string bytes) (ticket %receive nat)))
                (pair %send (contract %destination (ticket nat)) (nat %ticket_id))) ;
          storage
            (pair (address %admin)
                  (pair (big_map %tickets nat (ticket nat))
                        (pair (nat %current_id) (big_map %token_metadata nat (pair nat (map string bytes)))))) ;
          code { PUSH mutez 0 ;
                 AMOUNT ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 SWAP ;
                 UNPAIR ;
                 DIG 4 ;
                 IF_LEFT
                   { IF_LEFT
                       { IF_LEFT
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             NONE (ticket nat) ;
                             DIG 2 ;
                             DUP ;
                             DUG 3 ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             CDR ;
                             GET_AND_UPDATE ;
                             IF_NONE
                               { DROP 5 ; PUSH string "no tickets" ; FAILWITH }
                               { DIG 2 ;
                                 DUP ;
                                 DUG 3 ;
                                 CAR ;
                                 PUSH mutez 0 ;
                                 DIG 2 ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 DUP ;
                                 DUG 5 ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 DIG 4 ;
                                 CDR ;
                                 CAR ;
                                 PAIR ;
                                 TRANSFER_TOKENS ;
                                 DIG 3 ;
                                 DIG 3 ;
                                 PAIR ;
                                 DIG 2 ;
                                 PAIR ;
                                 DIG 2 ;
                                 PAIR ;
                                 NIL operation ;
                                 DIG 2 ;
                                 CONS ;
                                 PAIR } }
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             DIG 2 ;
                             NONE (ticket nat) ;
                             DIG 3 ;
                             UPDATE ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR } }
                       { IF_LEFT
                           { DIG 4 ;
                             DUP ;
                             DUG 5 ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 3 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             TICKET ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             GET_AND_UPDATE ;
                             DROP ;
                             DIG 3 ;
                             DIG 2 ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             PAIR ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             UPDATE ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR }
                           { READ_TICKET ;
                             UNPAIR ;
                             DROP ;
                             UNPAIR ;
                             DROP ;
                             DIG 4 ;
                             DIG 2 ;
                             SOME ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             GET_AND_UPDATE ;
                             DROP ;
                             PUSH nat 1 ;
                             DIG 2 ;
                             COMPARE ;
                             EQ ;
                             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                             DIG 2 ;
                             PUSH nat 1 ;
                             DIG 3 ;
                             ADD ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             SWAP ;
                             PAIR ;
                             NIL operation ;
                             PAIR } } }
                   { DIG 4 ;
                     DUP ;
                     DUG 5 ;
                     SENDER ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                     DIG 3 ;
                     NONE (ticket nat) ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     CDR ;
                     GET_AND_UPDATE ;
                     IF_NONE
                       { DROP 5 ; PUSH string "no tickets" ; FAILWITH }
                       { DIG 2 ;
                         CAR ;
                         PUSH mutez 0 ;
                         DIG 2 ;
                         TRANSFER_TOKENS ;
                         DIG 3 ;
                         DIG 3 ;
                         PAIR ;
                         DIG 2 ;
                         PAIR ;
                         DIG 2 ;
                         PAIR ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } } } }
        Initial storage:
          (Pair "tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y" (Pair {} (Pair 0 {})))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs
        Storage size: 1468 bytes
        Updated big_maps:
          New map(143) of type (big_map nat (pair nat (map string bytes)))
          New map(142) of type (big_map nat (ticket nat))
        Paid storage size diff: 1468 bytes
        Consumed gas: 7763.475
        Balance updates:
          tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ... -ꜩ0.367
          tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ... -ꜩ0.06425

New contract KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs originated.
The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opTMzFwbY9HvmQpTff5ViujAFVqguz5TwZE9FoKWmGT9onsnCmq to be included --confirmations 30 --branch BL6uqjE8sD1yM7dtNxWzN6DqKWJb3CSMe5vpTiPYA1hqZuunbZH
and/or an external block explorer.
Contract memorized as nft-wallet-bob.
```
## 7. After the time of one round has passed without anyone buying the nft, Alice drops the price of her nft to 90 mutez.

```
tezos-client transfer 0 from alice to nft-auction --entrypoint "drop_price" --arg 90 --burn-cap


Waiting for the node to be bootstrapped...
Current head: BLxDjvZH2LVC (timestamp: 2021-01-06T01:52:58.000-00:00, validation: 2021-01-06T01:53:18.405-00:00)
Node is bootstrapped.
Estimated gas: 13244.841 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooPRGpr5VxKuLiVtpf2XTAfrRFKV12nYS92zu1WLEaiqSp6mTpd'
Waiting for the operation to be included...
Operation found in block: BLrEgcoRiqTBQ4FYnnoZUhdN8XqxqSbqYTfLuUKcnSfEgYfnbvB (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
    Fee to the baker: ꜩ0.001604
    Expected counter: 103197
    Gas limit: 13345
    Storage limit: 0 bytes
    Balance updates:
      tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ............ -ꜩ0.001604
      fees(tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9,50) ... +ꜩ0.001604
    Transaction:
      Amount: ꜩ0
      From: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
      To: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
      Entrypoint: drop_price
      Parameter: 90
      This transaction was successfully applied
      Updated storage:
        (Pair { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ;
                90 ;
                10 ;
                True ;
                1609898008 ;
                600 }
              139)
      Storage size: 2383 bytes
      Consumed gas: 13244.841

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for ooPRGpr5VxKuLiVtpf2XTAfrRFKV12nYS92zu1WLEaiqSp6mTpd to be included --confirmations 30 --branch BLxDjvZH2LVC9StR6oUvP131Yj27zrn1xCZ8D3spz16ApxBxuCS
and/or an external block explorer.
```
## 8. Bob buys the nft by sending 90 mutez to the auction contract, calling the buy entrypoint, and sending the address of his wallet contract. The nft is sent to Bob's wallet and Alice is sent the 90 mutez.
```
$ tezos-client transfer 0.00009 from bob to nft-auction --entrypoint "buy" --arg "\"KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs%receive\"" --burn-cap 1

Waiting for the node to be bootstrapped...
Current head: BKkprtNFhPvu (timestamp: 2021-01-06T01:56:58.000-00:00, validation: 2021-01-06T01:57:02.519-00:00)
Node is bootstrapped.
Estimated gas: 31703.675 units (will add 100 for safety)
Estimated storage: 100 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oopZR4d3gjdbt21UKBDGYJtZtjZkACU6QFWhRN5yPisQiCQU5Df'
Waiting for the operation to be included...
Operation found in block: BLz11WgtY8kzmif7weCJ8Qucgc4dEbHJUUy7RcHf2j5nDvXt6FH (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
    Fee to the baker: ꜩ0.00349
    Expected counter: 103191
    Gas limit: 31804
    Storage limit: 120 bytes
    Balance updates:
      tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ............ -ꜩ0.00349
      fees(tz1Na5QB98cDA3BC1SQU4w3iiWGVGktU14LE,50) ... +ꜩ0.00349
    Transaction:
      Amount: ꜩ0.00009
      From: tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y
      To: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
      Entrypoint: buy
      Parameter: "KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs%receive"
      This transaction was successfully applied
      Updated storage:
        (Pair { 0x00000d4f0cf2fae2437f924120ef030f53abd4d4e520 ;
                90 ;
                10 ;
                False ;
                1609898008 ;
                600 }
              139)
      Updated big_maps:
        Unset map(139)[0]
      Storage size: 2283 bytes
      Consumed gas: 20805.897
      Balance updates:
        tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ... -ꜩ0.00009
        KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG ... +ꜩ0.00009
    Internal operations:
      Transaction:
        Amount: ꜩ0.00009
        From: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
        To: tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt
        This transaction was successfully applied
        Consumed gas: 1427
        Balance updates:
          KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG ... -ꜩ0.00009
          tz1LrQB9HrJcUaD9NKEvV65tnaiU8trPXwmt ... +ꜩ0.00009
      Transaction:
        Amount: ꜩ0
        From: KT1HWaMyNmjVGMBPUSm3QxJnFRLi4LQJi1tG
        To: KT1QQukCBULzFu6samB5FpbLNBXmNzArSpTs
        Entrypoint: receive
        Parameter: (Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1))
        This transaction was successfully applied
        Updated storage:
          { 0x0000b2d8083a660b2a77efe28a71bf09a933cd85613b ; 142 ; 1 ; 143 }
        Updated big_maps:
          Set map(142)[0] to (Pair 0x013d32a903fa4cf753f0d7075a7ab8395c501a2caf00 (Pair 0 1))
        Storage size: 1568 bytes
        Paid storage size diff: 100 bytes
        Consumed gas: 9470.778
        Balance updates:
          tz1bwfmSYqrhUTAoybGdhWBBefsbuhNdcC2Y ... -ꜩ0.025

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for oopZR4d3gjdbt21UKBDGYJtZtjZkACU6QFWhRN5yPisQiCQU5Df to be included --confirmations 30 --branch BKkprtNFhPvuUWTDn1mfML8TTPTYCeVX6vjHWPfGPCQ2nQrbSP4
and/or an external block explorer.
```
