import BigNumber from 'bignumber.js';
import { Context } from '../context';
import { OperationEmitter } from '../operations/operation-emitter';
import { Operation } from '../operations/operations';
import { TzProvider } from './interface';
export declare class RpcTzProvider extends OperationEmitter implements TzProvider {
    constructor(context: Context);
    getBalance(address: string): Promise<BigNumber>;
    getDelegate(address: string): Promise<string | null>;
    activate(pkh: string, secret: string): Promise<Operation>;
}
