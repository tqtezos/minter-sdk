"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Estimate = void 0;
var MINIMAL_FEE_MUTEZ = 100;
var MINIMAL_FEE_PER_BYTE_MUTEZ = 1;
var MINIMAL_FEE_PER_GAS_MUTEZ = 0.1;
var GAS_BUFFER = 100;
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
var Estimate = /** @class */ (function () {
    function Estimate(_milligasLimit, _storageLimit, opSize, minimalFeePerStorageByteMutez, 
    /**
     * @description Base fee in mutez (1 mutez = 1e10−6 tez)
     */
    baseFeeMutez) {
        if (baseFeeMutez === void 0) { baseFeeMutez = MINIMAL_FEE_MUTEZ; }
        this._milligasLimit = _milligasLimit;
        this._storageLimit = _storageLimit;
        this.opSize = opSize;
        this.minimalFeePerStorageByteMutez = minimalFeePerStorageByteMutez;
        this.baseFeeMutez = baseFeeMutez;
    }
    Object.defineProperty(Estimate.prototype, "burnFeeMutez", {
        /**
         * @description The number of Mutez that will be burned for the storage of the [operation](https://tezos.gitlab.io/user/glossary.html#operations). (Storage + Allocation fees)
         */
        get: function () {
            return this.roundUp(Number(this.storageLimit) * Number(this.minimalFeePerStorageByteMutez));
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "storageLimit", {
        /**
         * @description  The limit on the amount of storage an [operation](https://tezos.gitlab.io/user/glossary.html#operations) can use.
         */
        get: function () {
            var limit = Math.max(Number(this._storageLimit), 0);
            return limit > 0 ? limit : 0;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "gasLimit", {
        /**
         * @description The limit on the amount of [gas](https://tezos.gitlab.io/user/glossary.html#gas) a given operation can consume.
         */
        get: function () {
            return this.roundUp(Number(this._milligasLimit) / 1000 + GAS_BUFFER);
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "operationFeeMutez", {
        get: function () {
            return ((Number(this._milligasLimit) / 1000 + GAS_BUFFER) * MINIMAL_FEE_PER_GAS_MUTEZ + Number(this.opSize) * MINIMAL_FEE_PER_BYTE_MUTEZ);
        },
        enumerable: false,
        configurable: true
    });
    Estimate.prototype.roundUp = function (nanotez) {
        return Math.ceil(Number(nanotez));
    };
    Object.defineProperty(Estimate.prototype, "minimalFeeMutez", {
        /**
         * @description Minimum fees for the [operation](https://tezos.gitlab.io/user/glossary.html#operations) according to [baker](https://tezos.gitlab.io/user/glossary.html#baker) defaults.
         */
        get: function () {
            return this.roundUp(MINIMAL_FEE_MUTEZ + this.operationFeeMutez);
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "suggestedFeeMutez", {
        /**
         * @description The suggested fee for the operation which includes minimal fees and a small buffer.
         */
        get: function () {
            return this.roundUp(this.operationFeeMutez + MINIMAL_FEE_MUTEZ * 2);
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "usingBaseFeeMutez", {
        /**
         * @description Fees according to your specified base fee will ensure that at least minimum fees are used.
         */
        get: function () {
            return (Math.max(Number(this.baseFeeMutez), MINIMAL_FEE_MUTEZ) + this.roundUp(this.operationFeeMutez));
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "totalCost", {
        /**
         * @description The sum of `minimalFeeMutez` + `burnFeeMutez`.
         */
        get: function () {
            return this.minimalFeeMutez + this.burnFeeMutez;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Estimate.prototype, "consumedMilligas", {
        /**
         * @description Since Delphinet, consumed gas is provided in milligas for more precision.
         * This function returns an estimation of the gas that operation will consume in milligas.
         */
        get: function () {
            return Number(this._milligasLimit);
        },
        enumerable: false,
        configurable: true
    });
    return Estimate;
}());
exports.Estimate = Estimate;
//# sourceMappingURL=estimate.js.map