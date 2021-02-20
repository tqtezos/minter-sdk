import { OperationContentsAndResult, OperationContentsAndResultReveal } from '@taquito/rpc';
import { Context } from '../context';
import { ForgedBytes } from './types';
/**
 * @description Utility class to interact with Tezos operations
 */
export declare class Operation {
    readonly hash: string;
    readonly raw: ForgedBytes;
    readonly results: OperationContentsAndResult[];
    protected readonly context: Context;
    private _pollingConfig$;
    private _currentHeadPromise;
    private currentHead$;
    private polling$;
    private confirmed$;
    protected _foundAt: number;
    get includedInBlock(): number;
    /**
     *
     * @param hash Operation hash
     * @param raw Raw operation that was injected
     * @param context Taquito context allowing access to rpc and signer
     */
    constructor(hash: string, raw: ForgedBytes, results: OperationContentsAndResult[], context: Context);
    get revealOperation(): false | OperationContentsAndResultReveal | undefined;
    get revealStatus(): "applied" | "failed" | "skipped" | "backtracked" | "unknown";
    get status(): "applied" | "failed" | "skipped" | "backtracked" | "unknown";
    /**
     *
     * @param confirmations [0] Number of confirmation to wait for
     * @param interval [10] Polling interval
     * @param timeout [180] Timeout
     */
    confirmation(confirmations?: number, interval?: number, timeout?: number): Promise<number>;
}
