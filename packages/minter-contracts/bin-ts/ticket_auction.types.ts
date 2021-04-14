
import BigNumber from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';
type address = string;
type BigMap<K, T> = MichelsonMap<K, T>;
type contract = string;
type int = string | BigNumber | number;
type nat = string | BigNumber | number;
type ticket = string;
type timestamp = Date | string;

type Storage = {
    data: {
        admin: address;
        current_price: nat;
        reserve_price: nat;
        in_progress: boolean;
        start_time: timestamp;
        round_time: int;
    };
    tickets: BigMap<nat, ticket>;
};

type Methods = {
    buy: (param: contract) => Promise<void>;
    cancel: (param: contract) => Promise<void>;
    configure: (
        opening_price: nat,
        set_reserve_price: nat,
        set_start_time: timestamp,
        set_round_time: int,
        ticket: ticket,
    ) => Promise<void>;
    drop_price: (param: nat) => Promise<void>;
    start: () => Promise<void>;
};

export type TicketAuctionContractType = { methods: Methods, storage: Storage, code: { __type: 'TicketAuctionCode', protocol: string, code: object[] } };
