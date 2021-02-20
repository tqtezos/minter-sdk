import { Context } from '../context';
import { ContractMethod } from '../contract/contract';
import { EstimationProvider, ContractProvider } from '../contract/interface';
import { BatchOperation } from '../operations/batch-operation';
import { OperationEmitter } from '../operations/operation-emitter';
import { ActivationParams, DelegateParams, OriginateParams, TransferParams, ParamsWithKind } from '../operations/types';
import { OpKind } from '@taquito/rpc';
export declare const BATCH_KINDS: OpKind[];
export declare type BatchKinds = OpKind.ACTIVATION | OpKind.ORIGINATION | OpKind.TRANSACTION | OpKind.DELEGATION;
export declare class OperationBatch extends OperationEmitter {
    private estimator;
    private operations;
    constructor(context: Context, estimator: EstimationProvider);
    /**
     *
     * @description Add a transaction operation to the batch
     *
     * @param params Transfer operation parameter
     */
    withTransfer(params: TransferParams): this;
    /**
     *
     * @description Add a transaction operation to the batch
     *
     * @param params Transfer operation parameter
     */
    withContractCall(params: ContractMethod<ContractProvider>): this;
    /**
     *
     * @description Add a delegation operation to the batch
     *
     * @param params Delegation operation parameter
     */
    withDelegation(params: DelegateParams): this;
    /**
     *
     * @description Add an activation operation to the batch
     *
     * @param params Activation operation parameter
     */
    withActivation({ pkh, secret }: ActivationParams): this;
    /**
     *
     * @description Add an origination operation to the batch
     *
     * @param params Origination operation parameter
     */
    withOrigination(params: OriginateParams): this;
    private getRPCOp;
    /**
     *
     * @description Add a group operation to the batch. Operation will be applied in the order they are in the params array
     *
     * @param params Operations parameter
     */
    with(params: ParamsWithKind[]): this;
    /**
     *
     * @description Forge and Inject the operation batch
     *
     * @param params Optionally specify the source of the operation
     */
    send(params?: {
        source?: string;
    }): Promise<BatchOperation>;
}
export declare class RPCBatchProvider {
    private context;
    private estimator;
    constructor(context: Context, estimator: EstimationProvider);
    /***
     *
     * @description Batch a group of operation together. Operations will be applied in the order in which they are added to the batch
     *
     * @param params List of operation to batch together
     */
    batch(params?: ParamsWithKind[]): OperationBatch;
}
