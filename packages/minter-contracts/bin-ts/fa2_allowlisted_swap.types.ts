
import { address, BigMap, nat, unit } from './type-aliases';

type Storage = {
    admin: {
        admin: address;
        pending_admin?: address;
    };
    allowlist: BigMap<address, unit>;
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
};

type Methods = {
    confirm_admin: () => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    accept: (param: nat) => Promise<void>;
    cancel: (param: nat) => Promise<void>;
    start: (
        assets_offered: Array<{
            fa2_address: address;
            tokens: Array<{
                token_id: nat;
                amount: nat;
            }>;
        }>,
        assets_requested: Array<{
            fa2_address: address;
            tokens: Array<{
                token_id: nat;
                amount: nat;
            }>;
        }>,
    ) => Promise<void>;
    update_allowed: (param: BigMap<address, unit>) => Promise<void>;
};

export type Fa2AllowlistedSwapContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2AllowlistedSwapCode', protocol: string, code: object[] } };
