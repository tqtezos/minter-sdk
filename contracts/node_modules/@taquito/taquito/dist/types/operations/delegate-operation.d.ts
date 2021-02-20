import { OperationContentsAndResult } from '@taquito/rpc';
import { Context } from '../context';
import { Operation } from './operations';
import { FeeConsumingOperation, ForgedBytes, GasConsumingOperation, RPCDelegateOperation, StorageConsumingOperation } from './types';
/**
 * @description Delegation operation provide utility function to fetch newly issued delegation
 *
 * @warn Currently support only one delegation per operation
 */
export declare class DelegateOperation extends Operation implements GasConsumingOperation, StorageConsumingOperation, FeeConsumingOperation {
    private readonly params;
    readonly source: string;
    constructor(hash: string, params: RPCDelegateOperation, source: string, raw: ForgedBytes, results: OperationContentsAndResult[], context: Context);
    get operationResults(): import("@taquito/rpc").OperationResultDelegation | undefined;
    get status(): "applied" | "failed" | "skipped" | "backtracked" | "unknown";
    get delegate(): string;
    get isRegisterOperation(): boolean;
    get fee(): number;
    get gasLimit(): number;
    get storageLimit(): number;
    get consumedGas(): string | undefined;
    get errors(): import("@taquito/rpc").TezosGenericOperationError[] | undefined;
}
