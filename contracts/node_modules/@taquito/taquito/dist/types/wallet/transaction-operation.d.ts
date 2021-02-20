import { WalletOperation, OperationStatus } from './operation';
import { Context } from '../context';
import { Observable } from 'rxjs';
import { BlockResponse, OperationContentsAndResultReveal, OperationContentsAndResultTransaction } from '@taquito/rpc';
export declare class TransactionWalletOperation extends WalletOperation {
    readonly opHash: string;
    protected readonly context: Context;
    constructor(opHash: string, context: Context, newHead$: Observable<BlockResponse>);
    revealOperation(): Promise<OperationContentsAndResultReveal | undefined>;
    transactionOperation(): Promise<OperationContentsAndResultTransaction | undefined>;
    status(): Promise<OperationStatus>;
}
