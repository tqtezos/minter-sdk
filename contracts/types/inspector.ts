
type Storage = {
    0: {
        empty: void;
        state: {
            request: {
                owner: string;
                token_id: number;
            };
            balance: number;
        }[];
    };
};

type Methods = {
    default: () => Promise<void>;
    query: (params: {
        fa2: string;
        requests: {
                owner: string;
                token_id: number;
            }[];
    }) => Promise<void>;
    response: (params: {
        0: {
                request: {
                    owner: string;
                    token_id: number;
                };
                balance: number;
            }[];
        request: {
                owner: string;
                token_id: number;
            };
        balance: number;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
    