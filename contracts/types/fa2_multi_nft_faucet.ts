
import { address, BigMap, bytes, contract, MMap, nat, unit } from './type-aliases';

type Storage = {
    assets: {
        ledger: BigMap<nat, address>;
        next_token_id: nat;
        operators: BigMap<{
            0: address;
            1: address;
            2: nat;
        }, unit>;
        token_metadata: BigMap<nat, {
            token_id: nat;
            token_info: MMap<string, bytes>;
        }>;
    };
    metadata: BigMap<string, bytes>;
};

type Methods = {
    balance_of: (params: {
        requests: {
            owner: address;
            token_id: nat;
        }[];
        callback: contract;
    }) => Promise<void>;
    transfer: (params: {
        from_: address;
        txs: {
            to_: address;
            token_id: nat;
            amount: nat;
        }[];
    }) => Promise<void>;
    add_operator: (params: {
        owner: address;
        operator: address;
        token_id: nat;
    }) => Promise<void>;
    remove_operator: (params: {
        owner: address;
        operator: address;
        token_id: nat;
    }) => Promise<void>;
    mint: (params: {
        token_metadata: {
            token_id: nat;
            token_info: MMap<string, bytes>;
        };
        owner: address;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
