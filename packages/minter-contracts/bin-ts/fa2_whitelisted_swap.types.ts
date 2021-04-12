
import { address, BigMap, nat, unit } from './type-aliases';

type Storage = {
    admin: {
        admin: address;
        pending_admin?: address;
    };
    swap: {
        next_swap_id: nat;
        swaps: BigMap<nat, {
            swap_offer: {
                assets_offered: Array<{
                    fa2_address: address;
                    tokens: Array<{
                        token_id: nat;
                        amount: nat;
                    }>;
                }>;
                assets_requested: Array<{
                    fa2_address: address;
                    tokens: Array<{
                        token_id: nat;
                        amount: nat;
                    }>;
                }>;
            };
            seller: address;
        }>;
    };
    whitelist: BigMap<address, unit>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    accept: (param: nat) => Promise<void>;
    cancel: (param: nat) => Promise<void>;
    start: (params: {
        assets_offered: Array<{
            fa2_address: address;
            tokens: Array<{
                token_id: nat;
                amount: nat;
            }>;
        }>;
        assets_requested: Array<{
            fa2_address: address;
            tokens: Array<{
                token_id: nat;
                amount: nat;
            }>;
        }>;
    }) => Promise<void>;
    update_allowed: (params: {
        0: address;
    }) => Promise<void>;
};

export type Fa2WhitelistedSwapContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2WhitelistedSwapCode', protocol: string, code: unknown } };
