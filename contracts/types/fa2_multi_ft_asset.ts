
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };
type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };
type unit = ( true | undefined ) & { __type: 'unit' };

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    assets: {
        ledger: Map<{
            0: address;
            1: nat;
        }, nat>;
        operators: Map<{
            0: address;
            1: address;
            2: nat;
        }, unit>;
        token_metadata: Map<nat, {
            token_id: nat;
            token_info: Map<string, bytes>;
        }>;
        token_total_supply: Map<nat, nat>;
    };
    metadata: Map<string, bytes>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
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
    burn_tokens: (params: {
        owner: address;
        token_id: nat;
        amount: nat;
    }) => Promise<void>;
    create_token: (params: {
        token_id: nat;
        token_info: Map<string, bytes>;
    }) => Promise<void>;
    mint_tokens: (params: {
        owner: address;
        token_id: nat;
        amount: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
