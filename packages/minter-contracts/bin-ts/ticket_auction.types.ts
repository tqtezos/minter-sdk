
import { address, BigMap, contract, int, nat, ticket, timestamp } from './type-aliases';

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
