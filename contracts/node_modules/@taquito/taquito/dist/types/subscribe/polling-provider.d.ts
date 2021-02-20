import { Context } from '../context';
import { Filter, SubscribeProvider, Subscription, OperationContent } from './interface';
export declare class PollingSubscribeProvider implements SubscribeProvider {
    private context;
    readonly POLL_INTERVAL: number;
    private newBlock$;
    constructor(context: Context, POLL_INTERVAL?: number);
    subscribe(_filter: 'head'): Subscription<string>;
    subscribeOperation(filter: Filter): Subscription<OperationContent>;
}
