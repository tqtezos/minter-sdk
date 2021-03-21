
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
    auction: (params: {
        destination: contract;
        opening_price: nat;
        reserve_price: nat;
        start_time: timestamp;
        round_time: int;
        ticket_id: nat;
    }) => Promise<void>;
    burn: (param: nat) => Promise<void>;
    mint: (params: {
        0: string;
        1: bytes;
    }) => Promise<void>;
    receive: (param: nat) => Promise<void>;
    send: (params: {
        destination: contract;
        ticket_id: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
