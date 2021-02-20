/**
 * Examples of use :
 *
 *  Estimate a transfer operation :
 * ```
 * // Assuming that provider and signer are already configured...
 *
 * const amount = 2;
 * const address = 'tz1h3rQ8wBxFd8L9B3d7Jhaawu6Z568XU3xY';
 *
 * // Estimate gasLimit, storageLimit and fees for a transfer operation
 * const est = await Tezos.estimate.transfer({ to: address, amount: amount })
 * console.log(est.burnFeeMutez, est.gasLimit, est.minimalFeeMutez, est.storageLimit,
 *  est.suggestedFeeMutez, est.totalCost, est.usingBaseFeeMutez)
 *
 * ```
 *
 * Estimate a contract origination :
 * ```
 * // generic.json is referring to a Michelson Smart Contract
 *
 * const genericMultisigJSON = require('./generic.json')
 * const est = await Tezos.estimate.originate({
 *   code: genericMultisigJSON,
 *   storage: {
 *     stored_counter: 0,
 *     threshold: 1,
 *     keys: ['edpkuLxx9PQD8fZ45eUzrK3BhfDZJHhBuK4Zi49DcEGANwd2rpX82t']
 *   }
 * })
 * console.log(est.burnFeeMutez, est.gasLimit, est.minimalFeeMutez, est.storageLimit,
 *   est.suggestedFeeMutez, est.totalCost, est.usingBaseFeeMutez)
 *
 * ```
 */
export declare class Estimate {
    private readonly _milligasLimit;
    private readonly _storageLimit;
    private readonly opSize;
    private readonly minimalFeePerStorageByteMutez;
    /**
     * @description Base fee in mutez (1 mutez = 1e10−6 tez)
     */
    private readonly baseFeeMutez;
    constructor(_milligasLimit: number | string, _storageLimit: number | string, opSize: number | string, minimalFeePerStorageByteMutez: number | string, 
    /**
     * @description Base fee in mutez (1 mutez = 1e10−6 tez)
     */
    baseFeeMutez?: number | string);
    /**
     * @description The number of Mutez that will be burned for the storage of the [operation](https://tezos.gitlab.io/user/glossary.html#operations). (Storage + Allocation fees)
     */
    get burnFeeMutez(): number;
    /**
     * @description  The limit on the amount of storage an [operation](https://tezos.gitlab.io/user/glossary.html#operations) can use.
     */
    get storageLimit(): number;
    /**
     * @description The limit on the amount of [gas](https://tezos.gitlab.io/user/glossary.html#gas) a given operation can consume.
     */
    get gasLimit(): number;
    private get operationFeeMutez();
    private roundUp;
    /**
     * @description Minimum fees for the [operation](https://tezos.gitlab.io/user/glossary.html#operations) according to [baker](https://tezos.gitlab.io/user/glossary.html#baker) defaults.
     */
    get minimalFeeMutez(): number;
    /**
     * @description The suggested fee for the operation which includes minimal fees and a small buffer.
     */
    get suggestedFeeMutez(): number;
    /**
     * @description Fees according to your specified base fee will ensure that at least minimum fees are used.
     */
    get usingBaseFeeMutez(): number;
    /**
     * @description The sum of `minimalFeeMutez` + `burnFeeMutez`.
     */
    get totalCost(): number;
    /**
     * @description Since Delphinet, consumed gas is provided in milligas for more precision.
     * This function returns an estimation of the gas that operation will consume in milligas.
     */
    get consumedMilligas(): number;
}
