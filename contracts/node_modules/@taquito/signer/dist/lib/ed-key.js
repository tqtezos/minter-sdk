"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.Tz1 = void 0;
var libsodium_wrappers_1 = require("libsodium-wrappers");
var utils_1 = require("@taquito/utils");
var typedarray_to_buffer_1 = require("typedarray-to-buffer");
/**
 * @description Provide signing logic for ed25519 curve based key (tz1)
 */
var Tz1 = /** @class */ (function () {
    /**
     *
     * @param key Encoded private key
     * @param encrypted Is the private key encrypted
     * @param decrypt Decrypt function
     */
    function Tz1(key, encrypted, decrypt) {
        this.key = key;
        var keyPrefix = key.substr(0, encrypted ? 5 : 4);
        if (!utils_1.isValidPrefix(keyPrefix)) {
            throw new Error('key contains invalid prefix');
        }
        this._key = decrypt(utils_1.b58cdecode(this.key, utils_1.prefix[keyPrefix]));
        this._publicKey = this._key.slice(32);
        if (!this._key) {
            throw new Error('Unable to decode key');
        }
        this.isInit = this.init();
    }
    Tz1.prototype.init = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _a, publicKey, privateKey;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0: return [4 /*yield*/, libsodium_wrappers_1.default.ready];
                    case 1:
                        _b.sent();
                        if (this._key.length !== 64) {
                            _a = libsodium_wrappers_1.default.crypto_sign_seed_keypair(new Uint8Array(this._key), 'uint8array'), publicKey = _a.publicKey, privateKey = _a.privateKey;
                            this._publicKey = publicKey;
                            this._key = privateKey;
                        }
                        return [2 /*return*/, true];
                }
            });
        });
    };
    /**
     *
     * @param bytes Bytes to sign
     * @param bytesHash Blake2b hash of the bytes to sign
     */
    Tz1.prototype.sign = function (bytes, bytesHash) {
        return __awaiter(this, void 0, void 0, function () {
            var signature, signatureBuffer, sbytes;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.isInit];
                    case 1:
                        _a.sent();
                        signature = libsodium_wrappers_1.default.crypto_sign_detached(new Uint8Array(bytesHash), new Uint8Array(this._key));
                        signatureBuffer = typedarray_to_buffer_1.default(signature);
                        sbytes = bytes + utils_1.buf2hex(signatureBuffer);
                        return [2 /*return*/, {
                                bytes: bytes,
                                sig: utils_1.b58cencode(signature, utils_1.prefix.sig),
                                prefixSig: utils_1.b58cencode(signature, utils_1.prefix.edsig),
                                sbytes: sbytes,
                            }];
                }
            });
        });
    };
    /**
     * @returns Encoded public key
     */
    Tz1.prototype.publicKey = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.isInit];
                    case 1:
                        _a.sent();
                        return [2 /*return*/, utils_1.b58cencode(this._publicKey, utils_1.prefix['edpk'])];
                }
            });
        });
    };
    /**
     * @returns Encoded public key hash
     */
    Tz1.prototype.publicKeyHash = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.isInit];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, libsodium_wrappers_1.default.ready];
                    case 2:
                        _a.sent();
                        return [2 /*return*/, utils_1.b58cencode(libsodium_wrappers_1.default.crypto_generichash(20, new Uint8Array(this._publicKey)), utils_1.prefix.tz1)];
                }
            });
        });
    };
    /**
     * @returns Encoded private key
     */
    Tz1.prototype.secretKey = function () {
        return __awaiter(this, void 0, void 0, function () {
            var key, privateKey;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.isInit];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, libsodium_wrappers_1.default.ready];
                    case 2:
                        _a.sent();
                        key = this._key;
                        privateKey = libsodium_wrappers_1.default.crypto_sign_seed_keypair(new Uint8Array(key).slice(0, 32), 'uint8array').privateKey;
                        key = typedarray_to_buffer_1.default(privateKey);
                        return [2 /*return*/, utils_1.b58cencode(key, utils_1.prefix["edsk"])];
                }
            });
        });
    };
    return Tz1;
}());
exports.Tz1 = Tz1;
//# sourceMappingURL=ed-key.js.map