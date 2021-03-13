
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };
type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    assets: {
        ledger: Map<nat, address>;
        next_token_id: nat;
        operators: Map<{
            0: address;
            1: address;
            2: nat;
        }, void>;
        token_metadata: Map<nat, {
            token_id: nat;
            token_info: Map<string, bytes>;
        }>;
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
    mint: (params: {
        0: {
            token_metadata: {
                token_id: nat;
                token_info: Map<string, bytes>;
            };
            owner: address;
        }[];
        token_metadata: {
            token_id: nat;
            token_info: Map<string, bytes>;
        };
        owner: address;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
