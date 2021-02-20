import { TezosToolkit } from '@taquito/taquito';
/**
 *
 * @description Import a key to sign operation with the side-effect of setting the Tezos instance to use the InMemorySigner provider
 *
 * @param toolkit The toolkit instance to attach a signer
 * @param privateKeyOrEmail Key to load in memory
 * @param passphrase If the key is encrypted passphrase to decrypt it
 * @param mnemonic Faucet mnemonic
 * @param secret Faucet secret
 */
export declare function importKey(toolkit: TezosToolkit, privateKeyOrEmail: string, passphrase?: string, mnemonic?: string, secret?: string): Promise<void>;
