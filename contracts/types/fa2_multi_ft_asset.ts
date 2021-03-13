
type Storage = {
    admin?: {
        admin: string;
        paused: boolean;
        pending_admin?: string;
    };
    assets: {
        ledger: Map<{
            0: string;
            1: number;
        }, number>;
        operators: Map<{
            0: string;
            1: string;
            2: number;
        }, void>;
        token_metadata: Map<number, {
            token_id: number;
            token_info: Map<string, string>;
        }>;
        token_total_supply: Map<number, number>;
    };
    metadata: Map<string, string>;
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
    burn_tokens: (params: {
        0: {
            owner: string;
            token_id: number;
            amount: number;
        }[];
        owner: string;
        token_id: number;
        amount: number;
    }) => Promise<void>;
    create_token: (params: {
        token_id: number;
        token_info: Map<string, string>;
    }) => Promise<void>;
    mint_tokens: (params: {
        0: {
            owner: string;
            token_id: number;
            amount: number;
        }[];
        owner: string;
        token_id: number;
        amount: number;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
