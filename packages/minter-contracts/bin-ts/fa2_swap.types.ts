
import BigNumber from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';
type address = string;
type BigMap<K, T> = MichelsonMap<K, T>;
type nat = string | BigNumber | number;

type Storage = {
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

type Methods = {
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
};

export type Fa2SwapContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2SwapCode', protocol: string, code: object[] } };
