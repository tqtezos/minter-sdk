import { BigNumber } from 'bignumber.js';
import { ContractAbstraction, ContractMethod } from '@taquito/taquito/dist/types/contract';
import { ContractProvider } from '@taquito/taquito/dist/types/contract/interface';
import { Wallet } from '@taquito/taquito';

export type Contract<TContract extends { methods: unknown; storage: unknown; }>
  = WithTypedMethods<ContractAbstraction<ContractProvider>, TContract>;

type WithTypedMethods<T, TContract extends { methods: unknown; storage: unknown; }>
  = T extends { methods: any } ? Omit<T, 'methods'> & { methods:ContractMethodsOf<ContractProvider, TContract> } : never;

type ContractMethodsOf<
T extends ContractProvider | Wallet,
TContract extends { methods: unknown, storage: unknown }> = {
  [M in keyof TContract['methods']]:
  TContract['methods'][M] extends (...args: infer A) => unknown
  ? (...args: A) => ContractMethod<T>
  : never
};

export type address = string;
export type nat = BigNumber;
export type timestamp = string;
export type mutez = nat;
export type bytes = string;
