
type Storage = {
    pauseable_admin?: {
        admin: string;
        paused: boolean;
        pending_admin?: string;
    };
    current_id: number;
    max_auction_time: number;
    max_config_to_start_time: number;
    auctions: Map<number, {
        seller: string;
        current_bid: number;
        start_time: number;
        last_bid_time: number;
        round_time: number;
        extend_time: number;
        asset: {
            fa2_address: string;
            fa2_batch: {
                token_id: number;
                amount: number;
            }[];
        }[];
        min_raise_percent: number;
        min_raise: number;
        end_time: number;
        highest_bidder: string;
    }>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: string) => Promise<void>;
    bid: (param: number) => Promise<void>;
    cancel: (param: number) => Promise<void>;
    configure: (params: {
        opening_price: number;
        min_raise_percent: number;
        min_raise: number;
        round_time: number;
        extend_time: number;
        asset: {
            fa2_address: string;
            fa2_batch: {
                token_id: number;
                amount: number;
            }[];
        }[];
        start_time: number;
        end_time: number;
    }) => Promise<void>;
    resolve: (param: number) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
