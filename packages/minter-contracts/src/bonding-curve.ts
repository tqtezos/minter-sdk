import { Contract, address } from './type-aliases';

// import { MichelsonMapKey } from '@taquito/michelson-encoder';
// import { MichelsonMap, TezosToolkit, UnitValue } from '@taquito/taquito';
import { TezosToolkit } from '@taquito/taquito';

import { originateContract } from './ligo';
import {
  BondingCurveCode,
  BondingCurveContractType,
} from '../bin-ts';
// import { Storage as BondingCurveStorage } from "../bin-ts/bonding-curve.code"

import { $log } from '@tsed/logger';


export async function originateBondingCurve(
  tz: TezosToolkit,
  storage: string | Record<string, any>,
): Promise<Contract> {
  $log.info(`originating bonding curve contract..`);
  return originateContract(tz, BondingCurveCode.code, storage, 'bonding-curve');
}

export {
  BondingCurveCode,
  BondingCurveContractType,
} from '../bin-ts';

