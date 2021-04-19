
import { address, BigMap, nat } from './type-aliases';

type Storage = {
    admin: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    fee: {
        fee_address: address;
        fee_percent: nat;
    };
    sales: BigMap<{
        seller: address;
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
    buy: (
        seller: address,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
        money_token_address: address,
        money_token_token_id: nat,
    ) => Promise<void>;
    cancel: (
        seller: address,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
        money_token_address: address,
        money_token_token_id: nat,
    ) => Promise<void>;
    sell: (
        sale_price: nat,
        token_for_sale_address: address,
        token_for_sale_token_id: nat,
        money_token_address: address,
        money_token_token_id: nat,
    ) => Promise<void>;
};

export type FixedPriceSaleMarketFixedFeeContractType = { methods: Methods, storage: Storage, code: { __type: 'FixedPriceSaleMarketFixedFeeCode', protocol: string, code: object[] } };
