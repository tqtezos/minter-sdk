
import { address, BigMap, bytes, contract, MMap, nat, unit } from './type-aliases';

type Storage = {
    assets: {
        ledger: BigMap<{
            0: address;
            1: nat;
        }, nat>;
        operators: BigMap<{
            0: address;
            1: address;
            2: nat;
        }, unit>;
        token_metadata: BigMap<nat, {
            token_id: nat;
            token_info: MMap<string, bytes>;
        }>;
        token_total_supply: BigMap<nat, nat>;
    };
    metadata: BigMap<string, bytes>;
};

type Methods = {
    balance_of: (
        requests: Array<{
            owner: address;
            token_id: nat;
        }>,
        callback: contract,
    ) => Promise<void>;
    transfer: (
        from_: address,
        txs: Array<{
            to_: address;
            token_id: nat;
            amount: nat;
        }>,
    ) => Promise<void>;
    add_operator: (
        owner: address,
        operator: address,
        token_id: nat,
    ) => Promise<void>;
    remove_operator: (
        owner: address,
        operator: address,
        token_id: nat,
    ) => Promise<void>;
    burn_tokens: (
        owner: address,
        token_id: nat,
        amount: nat,
    ) => Promise<void>;
    create_token: (
        token_id: nat,
        token_info: MMap<string, bytes>,
    ) => Promise<void>;
    mint_tokens: (
        owner: address,
        token_id: nat,
        amount: nat,
    ) => Promise<void>;
};

export type Fa2MultiFtFaucetContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2MultiFtFaucetCode', protocol: string, code: unknown } };
