import { Observable } from 'rxjs';
import { Subscription } from './interface';
export declare class ObservableSubscription<T> implements Subscription<T> {
    private shouldRetry;
    private errorListeners;
    private messageListeners;
    private closeListeners;
    private completed$;
    constructor(obs: Observable<T>, shouldRetry?: boolean);
    private call;
    private remove;
    on(type: 'error', cb: (error: Error) => void): void;
    on(type: 'data', cb: (data: T) => void): void;
    on(type: 'close', cb: () => void): void;
    off(type: 'error', cb: (error: Error) => void): void;
    off(type: 'data', cb: (data: T) => void): void;
    off(type: 'close', cb: () => void): void;
    close(): void;
}
