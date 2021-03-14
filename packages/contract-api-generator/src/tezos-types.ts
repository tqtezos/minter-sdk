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
