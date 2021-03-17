
type address = string & { __type: 'address' };
type nat = number & { __type: 'nat' };
type unit = (true | undefined) & { __type: 'unit' };

type Storage = {
    0: (
        { empty: unit }
        | {
            state: {
                request: {
                    owner: address;
                    token_id: nat;
                };
                balance: nat;
            }[]
        }
    );
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
        request: {
            owner: address;
            token_id: nat;
        };
        balance: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
