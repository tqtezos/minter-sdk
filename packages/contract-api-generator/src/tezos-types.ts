import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

type address = string & { __type: 'address' };
type timestamp = string & { __type: 'timestamp' };

type nat = BigNumber & { __type: 'nat' };
type mutez = BigNumber & { __type: 'mutez' };
type tez = BigNumber & { __type: 'tez' };
type int = BigNumber & { __type: 'int' };

type contract = string & { __type: 'contract' };
type bytes = string & { __type: 'bytes' };

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
    return (value: number | string): T => new BigNumber(value) as T;
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
};