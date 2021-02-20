/**
 * @description Provide signing logic for ed25519 curve based key (tz1)
 */
export declare class Tz1 {
    private key;
    private _key;
    private _publicKey;
    private isInit;
    /**
     *
     * @param key Encoded private key
     * @param encrypted Is the private key encrypted
     * @param decrypt Decrypt function
     */
    constructor(key: string, encrypted: boolean, decrypt: (k: any) => any);
    private init;
    /**
     *
     * @param bytes Bytes to sign
     * @param bytesHash Blake2b hash of the bytes to sign
     */
    sign(bytes: string, bytesHash: Uint8Array): Promise<{
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
