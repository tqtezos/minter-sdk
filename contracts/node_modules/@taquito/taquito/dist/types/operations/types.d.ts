import { OperationObject, InternalOperationResultKindEnum, OpKind, TransactionOperationParameter } from '@taquito/rpc';
export { OpKind } from '@taquito/rpc';
export declare type withKind<T, K extends OpKind> = T & {
    kind: K;
};
export declare type ParamsWithKind = withKind<OriginateParams, OpKind.ORIGINATION> | withKind<DelegateParams, OpKind.DELEGATION> | withKind<TransferParams, OpKind.TRANSACTION> | withKind<ActivationParams, OpKind.ACTIVATION>;
export declare const attachKind: <T, K extends OpKind>(op: T, kind: K) => withKind<T, K>;
export declare const findWithKind: <T extends {
    kind: OpKind;
}, K extends OpKind>(arr: T[], kind: K) => (T & {
    kind: K;
}) | undefined;
export declare const isKind: <T extends {
    kind: OpKind;
}, K extends OpKind>(op: T, kind: K) => op is withKind<T, K>;
export declare type RPCOpWithFee = RPCTransferOperation | RPCOriginationOperation | RPCDelegateOperation | RPCRevealOperation;
export declare type RPCOpWithSource = RPCTransferOperation | RPCOriginationOperation | RPCDelegateOperation | RPCRevealOperation;
export declare const isOpWithFee: <T extends {
    kind: OpKind;
}>(op: T) => op is withKind<T, InternalOperationResultKindEnum>;
export declare const isOpRequireReveal: <T extends {
    kind: OpKind;
}>(op: T) => op is withKind<T, OpKind.ORIGINATION | OpKind.DELEGATION | OpKind.TRANSACTION>;
export declare type SourceKinds = InternalOperationResultKindEnum;
export declare const isSourceOp: <T extends {
    kind: OpKind;
}>(op: T) => op is withKind<T, InternalOperationResultKindEnum>;
export declare const hasMetadata: <T extends {
    kind: OpKind;
}, K>(op: T) => op is T & {
    metadata: K;
};
export declare const hasMetadataWithResult: <T extends {
    kind: OpKind;
}, K>(op: T) => op is T & {
    metadata: {
        operation_result: K;
    };
};
export declare const hasMetadataWithInternalOperationResult: <T extends {
    kind: OpKind;
}, K>(op: T) => op is T & {
    metadata: {
        internal_operation_results?: K | undefined;
    };
};
export interface GasConsumingOperation {
    consumedGas?: string;
    gasLimit: number;
}
export interface StorageConsumingOperation {
    storageDiff?: string;
    storageSize?: string;
    storageLimit: number;
}
export interface FeeConsumingOperation {
    fee: number;
}
export declare type OriginateParamsBase = {
    balance?: string;
    code: string | object[];
    delegate?: string;
    fee?: number;
    gasLimit?: number;
    storageLimit?: number;
};
/**
 * @description Parameters for originate method
 */
export declare type OriginateParams = OriginateParamsBase & ({
    init?: never;
    /** JS representation of a storage object */
    storage: any;
} | {
    /** Initial storage object value. Either Micheline or JSON encoded */
    init: string | object;
    storage?: never;
});
export interface ActivationParams {
    pkh: string;
    secret: string;
}
/**
 * @description RPC origination operation
 */
export interface RPCOriginationOperation {
    kind: OpKind.ORIGINATION;
    fee: number;
    gas_limit: number;
    storage_limit: number;
    balance: string;
    delegate?: string;
    source?: string;
    script: {
        code: any;
        storage: any;
    };
}
/**
 * @description RPC reveal operation
 */
export interface RPCRevealOperation {
    kind: OpKind.REVEAL;
    fee: number;
    public_key: string;
    source?: string;
    gas_limit: number;
    storage_limit: number;
}
/**
 * @description Result of a forge operation contains the operation plus its encoded version
 */
export interface ForgedBytes {
    opbytes: string;
    opOb: OperationObject;
    counter: number;
}
/**
 * @description Parameters for setDelegate method
 */
export interface DelegateParams {
    source: string;
    delegate: string;
    fee?: number;
    gasLimit?: number;
    storageLimit?: number;
}
/**
 * @description Parameters for registerDelegate method
 */
export interface RegisterDelegateParams {
    fee?: number;
    gasLimit?: number;
    storageLimit?: number;
}
/**
 * @description RPC delegation operation
 */
export interface RPCDelegateOperation {
    kind: OpKind.DELEGATION;
    source?: string;
    fee: number;
    gas_limit: number;
    storage_limit: number;
    delegate: string;
}
/**
 * @description Parameters for transfer method
 */
export interface TransferParams {
    to: string;
    source?: string;
    amount: number;
    fee?: number;
    parameter?: TransactionOperationParameter;
    gasLimit?: number;
    storageLimit?: number;
    mutez?: boolean;
}
/**
 * @description RPC transfer operation
 */
export interface RPCTransferOperation {
    kind: OpKind.TRANSACTION;
    fee: number;
    gas_limit: number;
    storage_limit: number;
    amount: string;
    source?: string;
    destination: string;
    parameters?: TransactionOperationParameter;
}
/**
 * @description RPC activate account operation
 */
export interface RPCActivateOperation {
    kind: OpKind.ACTIVATION;
    pkh: string;
    secret: string;
}
export declare type RPCOperation = RPCOriginationOperation | RPCTransferOperation | RPCDelegateOperation | RPCRevealOperation | RPCActivateOperation;
export declare type PrepareOperationParams = {
    operation: RPCOperation | RPCOperation[];
    source?: string;
};
