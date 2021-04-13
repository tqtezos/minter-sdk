import { BigNumber } from 'bignumber.js';
import { ContractAbstraction } from '@taquito/taquito/dist/types/contract';
import { ContractProvider } from '@taquito/taquito/dist/types/contract/interface';

export type Contract<TContract extends { methods: unknown; storage: unknown; }>
  = ContractAbstraction<ContractProvider<TContract>, TContract>;

export type address = string;
export type nat = BigNumber;
export type timestamp = string;
export type mutez = nat;
export type bytes = string;
