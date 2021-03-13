
type Storage = {
    admin?: {
        admin: string;
        paused: boolean;
        pending_admin?: string;
    };
    sales: { [key: {
        sale_seller: string;
        tokens: {
            token_for_sale_address: string;
            token_for_sale_token_id: number;
            money_token_address: string;
            money_token_token_id: number;
        };
    }]:number};
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: string) => Promise<void>;
    buy: (params: {
        sale_seller: string;
        tokens: {
                token_for_sale_address: string;
                token_for_sale_token_id: number;
                money_token_address: string;
                money_token_token_id: number;
            };
    }) => Promise<void>;
    cancel: (params: {
        sale_seller: string;
        tokens: {
                token_for_sale_address: string;
                token_for_sale_token_id: number;
                money_token_address: string;
                money_token_token_id: number;
            };
    }) => Promise<void>;
    sell: (params: {
        sale_price: number;
        sale_tokens_param: {
                token_for_sale_address: string;
                token_for_sale_token_id: number;
                money_token_address: string;
                money_token_token_id: number;
            };
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
    