
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };
type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };

type Storage = {
    assets: {
        ledger: Map<{
            0: address;
            1: nat;
        }, nat>;
        operators: Map<{
            0: address;
            1: address;
            2: nat;
        }, void>;
        token_metadata: Map<nat, {
            token_id: nat;
            token_info: Map<string, bytes>;
        }>;
        token_total_supply: Map<nat, nat>;
    };
    metadata: Map<string, bytes>;
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
        0: {
            from_: address;
            txs: {
                to_: address;
                token_id: nat;
                amount: nat;
            }[];
        }[];
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
    burn_tokens: (params: {
        0: {
            owner: address;
            token_id: nat;
            amount: nat;
        }[];
        owner: address;
        token_id: nat;
        amount: nat;
    }) => Promise<void>;
    create_token: (params: {
        token_id: nat;
        token_info: Map<string, bytes>;
    }) => Promise<void>;
    mint_tokens: (params: {
        0: {
            owner: address;
            token_id: nat;
            amount: nat;
        }[];
        owner: address;
        token_id: nat;
        amount: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
