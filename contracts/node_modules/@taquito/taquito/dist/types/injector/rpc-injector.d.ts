import { Injector } from './interface';
import { Context } from '../context';
export declare class RpcInjector implements Injector {
    private context;
    constructor(context: Context);
    inject(signedOperationBytes: string): Promise<string>;
}
