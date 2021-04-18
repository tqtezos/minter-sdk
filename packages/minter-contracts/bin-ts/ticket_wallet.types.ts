
import { address, BigMap, bytes, contract, int, MMap, nat, ticket, timestamp } from './type-aliases';

type Storage = {
    admin: address;
    tickets: BigMap<nat, ticket>;
    current_id: nat;
    token_metadata: BigMap<nat, {
        0: nat;
        1: MMap<string, bytes>;
    }>;
};

type Methods = {
    auction: (
        destination: contract,
        opening_price: nat,
        reserve_price: nat,
        start_time: timestamp,
        round_time: int,
        ticket_id: nat,
    ) => Promise<void>;
    burn: (param: nat) => Promise<void>;
    mint: (param: MMap<string, bytes>) => Promise<void>;
    receive: (param: ticket) => Promise<void>;
    send: (
        destination: contract,
        ticket_id: nat,
    ) => Promise<void>;
};

export type TicketWalletContractType = { methods: Methods, storage: Storage, code: { __type: 'TicketWalletCode', protocol: string, code: object[] } };
