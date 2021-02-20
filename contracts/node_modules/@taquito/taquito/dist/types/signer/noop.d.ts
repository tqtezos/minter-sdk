import { Signer } from './interface';
export declare class UnconfiguredSignerError implements Error {
    name: string;
    message: string;
}
/**
 * @description Default signer implementation which does nothing and produce invalid signature
 */
export declare class NoopSigner implements Signer {
    publicKey(): Promise<string>;
    publicKeyHash(): Promise<string>;
    secretKey(): Promise<string>;
    sign(_bytes: string, _watermark?: Uint8Array): Promise<any>;
}
