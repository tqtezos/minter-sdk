import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';
import * as MTypes from '../../../contracts/src/type-aliases';

export type address = MTypes.address & { __type: 'address' };
export type timestamp = MTypes.timestamp & { __type: 'timestamp' };
export type nat = MTypes.nat & { __type: 'nat' };
export type mutez = MTypes.mutez & { __type: 'mutez' };
export type tez = MTypes.mutez & { __type: 'tez' };
export type int = MTypes.nat & { __type: 'int' };
export type contract = MTypes.address & { __type: 'contract' };
export type bytes = MTypes.bytes & { __type: 'bytes' };


const createStringTypeTas = <T extends string>() => {
    return (value: string): T => value as T;
};

const createBigNumberTypeTas = <T extends BigNumber>() => {
    return (value: number | BigNumber | string): T => new BigNumber(value) as T;
};

type asMapParamOf<K, V> = K extends string ? { [key: string]: V } | { key: K, value: V }[]
    : K extends number ? { [key: number]: V } | { key: K, value: V }[]
    : { key: K, value: V }[];

function asMap<K, V>(value: asMapParamOf<K, V>): MichelsonMap<K, V> {
    const m = new MichelsonMap<K, V>();
    if (Array.isArray(value)) {
        const vArray = value as { key: K, value: V }[];
        vArray.forEach(x => m.set(x.key, x.value));
    } else {
        const vObject = value as { [key: string]: V };
        Object.keys(vObject).forEach(key => m.set(key as unknown as K, vObject[key]));
    }
    return m;
}

function add<T extends BigNumber>(a: T, b: T): T {
    return a.plus(b) as T;
}
function subtract<T extends BigNumber>(a: T, b: T): T {
    return a.minus(b) as T;
}

/** Tezos as casting for strict types */
export const tas = {
    address: createStringTypeTas<address>(),
    bytes: createStringTypeTas<bytes>(),
    contract: createStringTypeTas<contract>(),
    timestamp: (value: string | Date): timestamp => new Date(value).toISOString() as timestamp,

    int: createBigNumberTypeTas<int>(),
    nat: createBigNumberTypeTas<nat>(),
    mutez: createBigNumberTypeTas<mutez>(),
    tez: createBigNumberTypeTas<tez>(),

    map: asMap,

    // Operations
    add,
    subtract,
};