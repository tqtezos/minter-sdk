
import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

type address = string & { __type: 'address' };
type BigMap<K,V> = Omit<MichelsonMap<K, V>, 'get'> & { get: (key: K) => Promise<V> };
type bytes = string & { __type: 'bytes' };
type contract = string & { __type: 'contract' };
type MMap<K,V> = MichelsonMap<K,V>;
type nat = BigNumber & { __type: 'nat' };
type unit = (true | undefined) & { __type: 'unit' };

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
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
    mint: (params: {
        token_metadata: {
            token_id: nat;
            token_info: MMap<string, bytes>;
        };
        owner: address;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
