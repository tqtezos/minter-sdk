
import { address, BigMap, nat } from './type-aliases';

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    sales: BigMap<{
        sale_seller: address;
        tokens: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
            money_token_address: address;
            money_token_token_id: nat;
        };
    }, nat>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    buy: (params: {
        sale_seller: address;
        tokens: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
            money_token_address: address;
            money_token_token_id: nat;
        };
    }) => Promise<void>;
    cancel: (params: {
        sale_seller: address;
        tokens: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
            money_token_address: address;
            money_token_token_id: nat;
        };
    }) => Promise<void>;
    sell: (params: {
        sale_price: nat;
        sale_tokens_param: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
            money_token_address: address;
            money_token_token_id: nat;
        };
    }) => Promise<void>;
};

export type FixedPriceSaleMarketContractType = { methods: Methods, storage: Storage, code: { __type: 'FixedPriceSaleMarketCode', protocol: string, code: unknown } };
