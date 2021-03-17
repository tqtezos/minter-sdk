
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };
type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };
type unit = (true | undefined) & { __type: 'unit' };

type Storage = {
    assets: {
        ledger: Map<nat, address>;
        next_token_id: nat;
        operators: Map<{
            0: address;
            1: address;
            2: nat;
        }, unit>;
        token_metadata: Map<nat, {
            token_id: nat;
            token_info: Map<string, bytes>;
        }>;
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
            token_info: Map<string, bytes>;
        };
        owner: address;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
