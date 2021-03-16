import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

type address = string & { __type: 'address' };
type timestamp = string & { __type: 'timestamp' };

type nat = BigNumber & { __type: 'nat' };
type mutez = BigNumber & { __type: 'mutez' };
type tez = BigNumber & { __type: 'tez' };
type int = BigNumber & { __type: 'int' };

type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    assets: {
        ledger: MichelsonMap<{
            0: address;
            1: nat;
        }, nat>;
        operators: MichelsonMap<{
            0: address;
            1: address;
            2: nat;
        }, void>;
        token_metadata: MichelsonMap<nat, {
            token_id: nat;
            token_info: MichelsonMap<string, bytes>;
        }>;
        token_total_supply: MichelsonMap<nat, nat>;
    };
    metadata: MichelsonMap<string, bytes>;
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
        token_info: MichelsonMap<string, bytes>;
    }) => Promise<void>;
    mint_tokens: (params: {
        owner: address;
        token_id: nat;
        amount: nat;
    }) => Promise<void>;
};

export type TestContractType2 = { methods: Methods, storage: Storage };
