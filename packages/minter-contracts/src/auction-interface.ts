import { address, nat, timestamp, mutez } from './type-aliases';

export interface Fa2_token {
    token_id : nat;
    amount : nat;
}

export interface Tokens {
    fa2_address : address;
    fa2_batch : [Fa2_token];
}

export interface ConfigureAuctionParam {
    opening_price : mutez;
    min_raise : mutez;
    round_time : nat;
    asset : [Tokens];
    auction_time : nat;
    start_time : timestamp;
}

export const sleep = (ms: number) => new Promise(r => setTimeout(r, ms));
