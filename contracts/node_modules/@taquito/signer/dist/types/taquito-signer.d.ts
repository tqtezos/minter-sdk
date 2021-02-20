export * from './import-key';
/**
 * @description A local implementation of the signer. Will represent a Tezos account and be able to produce signature in its behalf
 *
 * @warn If running in production and dealing with tokens that have real value, it is strongly recommended to use a HSM backed signer so that private key material is not stored in memory or on disk
 *
 * @warn Calling this constructor directly is discouraged as it do not await for sodium library to be loaded.
 *
 * Consider doing:
 *
 * ```const sodium = require('libsodium-wrappers'); await sodium.ready;```
 *
 * The recommended usage is to use InMemorySigner.fromSecretKey('edsk', 'passphrase')
 */
export declare class InMemorySigner {
    private _key;
    static fromFundraiser(email: string, password: string, mnemonic: string): InMemorySigner;
    static fromSecretKey(key: string, passphrase?: string): Promise<InMemorySigner>;
    /**
     *
     * @param key Encoded private key
     * @param passphrase Passphrase to decrypt the private key if it is encrypted
     *
     */
    constructor(key: string, passphrase?: string);
    /**
     *
     * @param bytes Bytes to sign
     * @param watermark Watermark to append to the bytes
     */
    sign(bytes: string, watermark?: Uint8Array): Promise<{
        bytes: string;
        sig: any;
        prefixSig: any;
        sbytes: string;
    }>;
    /**
     * @returns Encoded public key
     */
    publicKey(): Promise<string>;
    /**
     * @returns Encoded public key hash
     */
    publicKeyHash(): Promise<string>;
    /**
     * @returns Encoded private key
     */
    secretKey(): Promise<string>;
}
