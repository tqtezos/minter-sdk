import { OperationContentsAndResult, OperationResultOrigination } from '@taquito/rpc';
import { Context } from '../context';
import { RpcContractProvider } from '../contract/rpc-contract-provider';
import { Operation } from './operations';
import { FeeConsumingOperation, ForgedBytes, GasConsumingOperation, RPCOriginationOperation, StorageConsumingOperation } from './types';
/**
 * @description Origination operation provide utility function to fetch newly originated contract
 *
 * @warn Currently support only one origination per operation
 */
export declare class OriginationOperation extends Operation implements GasConsumingOperation, StorageConsumingOperation, FeeConsumingOperation {
    private readonly params;
    private contractProvider;
    /**
     * @description Contract address of the newly originated contract
     */
    readonly contractAddress?: string;
    constructor(hash: string, params: RPCOriginationOperation, raw: ForgedBytes, results: OperationContentsAndResult[], context: Context, contractProvider: RpcContractProvider);
    get status(): "applied" | "failed" | "skipped" | "backtracked" | "unknown";
    get operationResults(): OperationResultOrigination | undefined;
    get fee(): number;
    get gasLimit(): number;
    get storageLimit(): number;
    get consumedGas(): string | undefined;
    get storageDiff(): string | undefined;
    get storageSize(): string | undefined;
    get errors(): import("@taquito/rpc").TezosGenericOperationError[] | undefined;
    /**
     * @description Provide the contract abstract of the newly originated contract
     */
    contract(confirmations?: number, interval?: number, timeout?: number): Promise<import("../contract").ContractAbstraction<import("../contract").ContractProvider>>;
}
