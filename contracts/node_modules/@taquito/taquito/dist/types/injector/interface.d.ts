export declare type InjectorParams = string;
export declare type TxHash = string;
export interface Injector {
    inject(signedOperationBytes: InjectorParams): Promise<TxHash>;
}
