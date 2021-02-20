import { OperationContentsAndResult, OperationContentsAndResultTransaction } from '@taquito/rpc';
import BigNumber from 'bignumber.js';
import { Context } from '../context';
import { Operation } from './operations';
import { FeeConsumingOperation, ForgedBytes, GasConsumingOperation, RPCTransferOperation, StorageConsumingOperation } from './types';
/**
 * @description Transaction operation provides utility functions to fetch a newly issued transaction
 *
 * @warn Currently supports one transaction per operation
 */
export declare class TransactionOperation extends Operation implements GasConsumingOperation, StorageConsumingOperation, FeeConsumingOperation {
    private readonly params;
    readonly source: string;
    constructor(hash: string, params: RPCTransferOperation, source: string, raw: ForgedBytes, results: OperationContentsAndResult[], context: Context);
    get operationResults(): OperationContentsAndResultTransaction[];
    get status(): "applied" | "failed" | "skipped" | "backtracked" | "unknown";
    get amount(): BigNumber;
    get destination(): string;
    get fee(): number;
    get gasLimit(): number;
    get storageLimit(): number;
    private sumProp;
    get consumedGas(): string;
    get storageDiff(): string;
    get storageSize(): string;
    get errors(): import("@taquito/rpc").TezosGenericOperationError[];
}
