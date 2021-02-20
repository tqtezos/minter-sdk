import { RpcClient } from '@taquito/rpc';
import { Protocols } from './constants';
import { Forger } from './forger/interface';
import { Injector } from './injector/interface';
import { Signer } from './signer/interface';
import { OperationFactory } from './wallet/opreation-factory';
import { RpcTzProvider } from './tz/rpc-tz-provider';
import { RPCEstimateProvider } from './contract/rpc-estimate-provider';
import { RpcContractProvider } from './contract/rpc-contract-provider';
import { RPCBatchProvider } from './batch/rpc-batch-provider';
import { Wallet, WalletProvider } from './wallet';
export interface TaquitoProvider<T, K extends Array<any>> {
    new (context: Context, ...rest: K): T;
}
export interface Config {
    confirmationPollingIntervalSecond?: number;
    confirmationPollingTimeoutSecond?: number;
    defaultConfirmationCount?: number;
    shouldObservableSubscriptionRetry?: boolean;
}
export declare const defaultConfig: Required<Config>;
/**
 * @description Encapsulate common service used throughout different part of the library
 */
export declare class Context {
    private _rpc;
    private _signer;
    private _proto?;
    private _config?;
    private _rpcClient;
    private _forger;
    private _injector;
    private _walletProvider;
    readonly operationFactory: OperationFactory;
    readonly tz: RpcTzProvider;
    readonly estimate: RPCEstimateProvider;
    readonly contract: RpcContractProvider;
    readonly batch: RPCBatchProvider;
    readonly wallet: Wallet;
    constructor(_rpc: RpcClient | string, _signer?: Signer, _proto?: Protocols | undefined, _config?: Partial<Config> | undefined, forger?: Forger, injector?: Injector, wallet?: WalletProvider);
    get config(): Required<Config>;
    set config(value: Required<Config>);
    get rpc(): RpcClient;
    set rpc(value: RpcClient);
    get injector(): Injector;
    set injector(value: Injector);
    get forger(): Forger;
    set forger(value: Forger);
    get signer(): Signer;
    get walletProvider(): WalletProvider;
    set walletProvider(value: WalletProvider);
    set signer(value: Signer);
    set proto(value: Protocols | undefined);
    get proto(): Protocols | undefined;
    isAnyProtocolActive(protocol?: string[]): Promise<boolean>;
    /**
     * @description Create a copy of the current context. Useful when you have long running operation and you do not want a context change to affect the operation
     */
    clone(): Context;
}
