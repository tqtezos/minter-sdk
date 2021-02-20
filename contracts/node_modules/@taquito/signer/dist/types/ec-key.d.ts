/**
 * @description Provide signing logic for elliptic curve based key (tz2, tz3)
 */
export declare class ECKey {
    private curve;
    private key;
    private _key;
    private _publicKey;
    /**
     *
     * @param curve Curve to use with the key
     * @param key Encoded private key
     * @param encrypted Is the private key encrypted
     * @param decrypt Decrypt function
     */
    constructor(curve: 'p256' | 'secp256k1', key: string, encrypted: boolean, decrypt: (k: any) => any);
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
/**
 * @description Tz3 key class using the p256 curve
 */
export declare const Tz3: new (key: string, encrypted: boolean, decrypt: (k: any) => any) => ECKey;
/**
 * @description Tz3 key class using the secp256k1 curve
 */
export declare const Tz2: new (key: string, encrypted: boolean, decrypt: (k: any) => any) => ECKey;
