
type Storage = {
    admin?: {
        admin: string;
        paused: boolean;
        pending_admin?: string;
    };
    sales: Map<{
        sale_seller: string;
        sale_token: {
            token_for_sale_address: string;
            token_for_sale_token_id: number;
        };
    }, number>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: string) => Promise<void>;
    buy: (params: {
        sale_seller: string;
        sale_token: {
            token_for_sale_address: string;
            token_for_sale_token_id: number;
        };
    }) => Promise<void>;
    cancel: (params: {
        sale_seller: string;
        sale_token: {
            token_for_sale_address: string;
            token_for_sale_token_id: number;
        };
    }) => Promise<void>;
    sell: (params: {
        sale_price: number;
        sale_token_param_tez: {
            token_for_sale_address: string;
            token_for_sale_token_id: number;
        };
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
