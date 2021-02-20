import { MichelsonV1ExpressionBase, OperationResultDelegation, OperationResultOrigination, OperationResultReveal, OperationResultTransaction, PreapplyResponse, TezosGenericOperationError } from '@taquito/rpc';
export interface TezosOperationErrorWithMessage extends TezosGenericOperationError {
    with: MichelsonV1ExpressionBase;
}
export declare class TezosOperationError implements Error {
    errors: TezosGenericOperationError[];
    name: string;
    id: string;
    kind: string;
    message: string;
    constructor(errors: TezosGenericOperationError[]);
}
export declare class TezosPreapplyFailureError implements Error {
    result: any;
    name: string;
    message: string;
    constructor(result: any);
}
export declare type MergedOperationResult = OperationResultDelegation & OperationResultOrigination & OperationResultTransaction & OperationResultReveal & {
    fee?: string;
};
export declare const flattenOperationResult: (response: PreapplyResponse | PreapplyResponse[]) => MergedOperationResult[];
/***
 * @description Flatten all error from preapply response (including internal error)
 */
export declare const flattenErrors: (response: PreapplyResponse | PreapplyResponse[], status?: string) => TezosGenericOperationError[];
