
import { address, BigMap, bytes, contract, MMap, nat, unit } from './type-aliases';

type Storage = {
    editions_metadata: BigMap<nat, {
        creator: address;
        edition_info: MMap<string, bytes>;
        number_of_editions: nat;
        number_of_editions_to_distribute: nat;
    }>;
    max_editions_per_run: nat;
    next_edition_id: nat;
    nft_asset_storage: {
        admin: {
            admin: address;
            paused: boolean;
            pending_admin?: address;
        };
        assets: {
            ledger: BigMap<nat, address>;
            operators: BigMap<{
                0: address;
                1: address;
                2: nat;
            }, unit>;
        };
        metadata: BigMap<string, bytes>;
    };
};

type Methods = {
    distribute_editions: (param: Array<{
            edition_id: nat;
            receivers: Array<address>;
        }>) => Promise<void>;
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    balance_of: (
        requests: Array<{
            owner: address;
            token_id: nat;
        }>,
        callback: contract,
    ) => Promise<void>;
    transfer: (param: Array<{
            from_: address;
            txs: Array<{
                to_: address;
                token_id: nat;
                amount: nat;
            }>;
        }>) => Promise<void>;
    add_operator: (
        owner: address,
        operator: address,
        token_id: nat,
    ) => Promise<void>;
    remove_operator: (
        owner: address,
        operator: address,
        token_id: nat,
    ) => Promise<void>;
    mint_editions: (param: Array<{
            edition_info: MMap<string, bytes>;
            number_of_editions: nat;
        }>) => Promise<void>;
};

export type Fa2MultiNftTokenEditionsContractType = { methods: Methods, storage: Storage, code: { __type: 'Fa2MultiNftTokenEditionsCode', protocol: string, code: object[] } };
