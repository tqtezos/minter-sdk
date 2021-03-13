
type Storage = {
    admin?: {
        admin: string;
        paused: boolean;
        pending_admin?: string;
    };
    assets: {
        ledger: {
            [key: number]: string;
        };
        next_token_id: number;
        operators: {
            [key: {
            0: string;
            1: string;
            2: number;
        }]: void;
        };
        token_metadata: {
            [key: number]: {
                token_id: number;
                token_info: {
                    [key: string]: string;
                };
            };
        };
    };
    metadata: {
        [key: string]: string;
    };
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: string) => Promise<void>;
    balance_of: (params: {
        requests: {
            owner: string;
            token_id: number;
        }[];
        callback: string;
    }) => Promise<void>;
    transfer: (params: {
        0: {
            from_: string;
            txs: {
                to_: string;
                token_id: number;
                amount: number;
            }[];
        }[];
        from_: string;
        txs: {
            to_: string;
            token_id: number;
            amount: number;
        }[];
    }) => Promise<void>;
    add_operator: (params: {
        owner: string;
        operator: string;
        token_id: number;
    }) => Promise<void>;
    remove_operator: (params: {
        owner: string;
        operator: string;
        token_id: number;
    }) => Promise<void>;
    mint: (params: {
        0: {
            token_metadata: {
                token_id: number;
                token_info: {
                    [key: string]: string;
                };
            };
            owner: string;
        }[];
        token_metadata: {
            token_id: number;
            token_info: {
                [key: string]: string;
            };
        };
        owner: string;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
