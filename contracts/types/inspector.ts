
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };

type Storage = {
    0: {
        empty: void;
        state: {
            request: {
                owner: address;
                token_id: nat;
            };
            balance: nat;
        }[];
    };
};

type Methods = {
    default: () => Promise<void>;
    query: (params: {
        fa2: address;
        requests: {
            owner: address;
            token_id: nat;
        }[];
    }) => Promise<void>;
    response: (params: {
        0: {
            request: {
                owner: address;
                token_id: nat;
            };
            balance: nat;
        }[];
        request: {
            owner: address;
            token_id: nat;
        };
        balance: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
