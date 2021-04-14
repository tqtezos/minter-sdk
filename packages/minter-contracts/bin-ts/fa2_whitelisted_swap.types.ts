
import BigNumber from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';
type address = string;
type BigMap<K, T> = MichelsonMap<K, T>;
type nat = string | BigNumber | number;
type unit = (true | undefined);

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

export type Fa2WhitelistedSwapContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2WhitelistedSwapCode', protocol: string, code: object[] } };
