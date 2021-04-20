
import { address, BigMap, mutez, nat, unit } from './type-aliases';

type Storage = {
    allowlist: BigMap<address, unit>;
    market_storage: {
        admin?: {
            admin: address;
            paused: boolean;
            pending_admin?: address;
        };
        sales: BigMap<{
            seller: address;
            sale_token: {
                token_for_sale_address: address;
                token_for_sale_token_id: nat;
            };
        }, mutez>;
    };
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    buy: (
        seller: address,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
    ) => Promise<void>;
    cancel: (
        seller: address,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
    ) => Promise<void>;
    sell: (
        sale_price: mutez,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
    ) => Promise<void>;
    update_allowed: (param: BigMap<address, unit>) => Promise<void>;
};

export type FixedPriceSaleMarketTezAllowlistedContractType = { methods: Methods, storage: Storage, code: { __type: 'FixedPriceSaleMarketTezAllowlistedCode', protocol: string, code: object[] } };
