import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

export type address = string & { __type: 'address' };
export type timestamp = string & { __type: 'timestamp' };
export type nat = BigNumber & { __type: 'nat' };
export type mutez = BigNumber & { __type: 'mutez' };
export type tez = BigNumber & { __type: 'tez' };
export type int = BigNumber & { __type: 'int' };
export type contract = string & { __type: 'contract' };
export type bytes = string & { __type: 'bytes' };

const createStringType = <T extends string>() => {
    return {
        fromString: (value: string): T => value as T,
        toString: (value: T): string => value as string,
    };
};

const createBigNumberType = <T extends BigNumber>() => {
    return {
        fromBigNumber: (value: BigNumber): T => value as T,
        fromNumber: (value: number): T => new BigNumber(value) as T,
        fromString: (value: string): T => new BigNumber(value) as T,
        toBigNumber: (value: T): BigNumber => value as BigNumber,
    };
};

export const TezosTypes = {
    address: createStringType<address>(),
    bytes: createStringType<bytes>(),
    contract: createStringType<contract>(),
    timestamp: {
        ...createStringType<timestamp>(),
        fromDate: (value: Date): timestamp => value.toISOString() as timestamp,
    },

    int: createBigNumberType<int>(),
    nat: createBigNumberType<nat>(),
    mutez: createBigNumberType<mutez>(),
    tez: createBigNumberType<tez>(),

    map: {
        from: <K, V>(value: { key: K, value: V }[]): MichelsonMap<K, V> => {
            const m = new MichelsonMap<K, V>();
            value.forEach(x => m.set(x.key, x.value));
            return m;
        },
    },
};


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