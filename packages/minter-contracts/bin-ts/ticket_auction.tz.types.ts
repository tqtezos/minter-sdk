
import { address, BigMap, int, nat, ticket, timestamp } from '@taquito/contract-type-generator';

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
    buy: (param: ticket) => Promise<void>;
    cancel: (param: ticket) => Promise<void>;
    configure: (params: {
        opening_price: nat;
        set_reserve_price: nat;
        set_start_time: timestamp;
        set_round_time: int;
        ticket: ticket;
    }) => Promise<void>;
    drop_price: (param: nat) => Promise<void>;
    start: () => Promise<void>;
};

export type TicketAuctionTzContractType = { methods: Methods, storage: Storage, code: { __type: 'TicketAuctionTzCode' } };
