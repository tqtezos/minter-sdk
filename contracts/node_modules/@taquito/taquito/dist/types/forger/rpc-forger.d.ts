import { Forger, ForgeParams, ForgeResponse } from './interface';
import { Context } from '../context';
export declare class RpcForger implements Forger {
    private context;
    constructor(context: Context);
    forge({ branch, contents }: ForgeParams): Promise<ForgeResponse>;
}
