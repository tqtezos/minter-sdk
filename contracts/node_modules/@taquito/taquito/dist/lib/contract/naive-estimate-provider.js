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
var __values = (this && this.__values) || function(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.NaiveEstimateProvider = void 0;
var constants_1 = require("../constants");
var estimate_1 = require("./estimate");
/**
 * @description Naïve implementation of an estimate provider. Will work for basic transaction but your operation risk to fail if they are more complex (smart contract interaction)
 */
var NaiveEstimateProvider = /** @class */ (function () {
    function NaiveEstimateProvider(protocol) {
        this.protocol = protocol;
        if (this.protocol === constants_1.Protocols.PsCARTHA) {
            this._costPerByte = 1000;
        }
        else {
            this._costPerByte = 250;
        }
    }
    /**
     *
     * @description Estimate gasLimit, storageLimit and fees for an origination operation
     *
     * @returns An estimation of gasLimit, storageLimit and fees for the operation
     *
     * @param OriginationOperation Originate operation parameter
     */
    NaiveEstimateProvider.prototype.originate = function (_a) {
        var _b = _a.fee, fee = _b === void 0 ? constants_1.DEFAULT_FEE.ORIGINATION : _b, _c = _a.storageLimit, storageLimit = _c === void 0 ? constants_1.DEFAULT_STORAGE_LIMIT.ORIGINATION : _c, _d = _a.gasLimit, gasLimit = _d === void 0 ? constants_1.DEFAULT_GAS_LIMIT.ORIGINATION * 1000 : _d;
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_e) {
                return [2 /*return*/, new estimate_1.Estimate(gasLimit, storageLimit, 185, this._costPerByte, fee)];
            });
        });
    };
    /**
     *
     * @description Estimate gasLimit, storageLimit and fees for an transfer operation
     *
     * @returns An estimation of gasLimit, storageLimit and fees for the operation
     *
     * @param TransferOperation Originate operation parameter
     */
    NaiveEstimateProvider.prototype.transfer = function (_a) {
        var _b = _a.fee, fee = _b === void 0 ? constants_1.DEFAULT_FEE.TRANSFER : _b, _c = _a.storageLimit, storageLimit = _c === void 0 ? constants_1.DEFAULT_STORAGE_LIMIT.TRANSFER : _c, _d = _a.gasLimit, gasLimit = _d === void 0 ? constants_1.DEFAULT_GAS_LIMIT.TRANSFER * 1000 : _d;
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_e) {
                return [2 /*return*/, new estimate_1.Estimate(gasLimit, storageLimit, 162, this._costPerByte, fee)];
            });
        });
    };
    /**
     *
     * @description Estimate gasLimit, storageLimit and fees for a delegate operation
     *
     * @returns An estimation of gasLimit, storageLimit and fees for the operation
     *
     * @param Estimate
     */
    NaiveEstimateProvider.prototype.setDelegate = function (_a) {
        var _b = _a.fee, fee = _b === void 0 ? constants_1.DEFAULT_FEE.DELEGATION : _b, _c = _a.gasLimit, gasLimit = _c === void 0 ? constants_1.DEFAULT_GAS_LIMIT.DELEGATION * 1000 : _c;
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_d) {
                return [2 /*return*/, new estimate_1.Estimate(gasLimit, 0, 157, this._costPerByte, fee)];
            });
        });
    };
    /**
     *
     * @description Estimate gasLimit, storageLimit and fees for a delegate operation
     *
     * @returns An estimation of gasLimit, storageLimit and fees for the operation
     *
     * @param Estimate
     */
    NaiveEstimateProvider.prototype.registerDelegate = function (_a) {
        var _b = _a.fee, fee = _b === void 0 ? constants_1.DEFAULT_FEE.DELEGATION : _b, _c = _a.gasLimit, gasLimit = _c === void 0 ? constants_1.DEFAULT_GAS_LIMIT.DELEGATION * 1000 : _c;
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_d) {
                return [2 /*return*/, new estimate_1.Estimate(gasLimit, 0, 157, this._costPerByte, fee)];
            });
        });
    };
    NaiveEstimateProvider.prototype.batch = function (params) {
        return __awaiter(this, void 0, void 0, function () {
            var estimates, params_1, params_1_1, param, _a, _b, _c, _d, _e, _f, _g, e_1_1;
            var e_1, _h;
            return __generator(this, function (_j) {
                switch (_j.label) {
                    case 0:
                        estimates = [];
                        _j.label = 1;
                    case 1:
                        _j.trys.push([1, 13, 14, 15]);
                        params_1 = __values(params), params_1_1 = params_1.next();
                        _j.label = 2;
                    case 2:
                        if (!!params_1_1.done) return [3 /*break*/, 12];
                        param = params_1_1.value;
                        _a = param.kind;
                        switch (_a) {
                            case 'transaction': return [3 /*break*/, 3];
                            case 'origination': return [3 /*break*/, 5];
                            case 'delegation': return [3 /*break*/, 7];
                            case 'activate_account': return [3 /*break*/, 9];
                        }
                        return [3 /*break*/, 10];
                    case 3:
                        _c = (_b = estimates).push;
                        return [4 /*yield*/, this.transfer(param)];
                    case 4:
                        _c.apply(_b, [_j.sent()]);
                        return [3 /*break*/, 11];
                    case 5:
                        _e = (_d = estimates).push;
                        return [4 /*yield*/, this.originate(param)];
                    case 6:
                        _e.apply(_d, [_j.sent()]);
                        return [3 /*break*/, 11];
                    case 7:
                        _g = (_f = estimates).push;
                        return [4 /*yield*/, this.setDelegate(param)];
                    case 8:
                        _g.apply(_f, [_j.sent()]);
                        return [3 /*break*/, 11];
                    case 9:
                        estimates.push(new estimate_1.Estimate(0, 0, 0, this._costPerByte, 0));
                        return [3 /*break*/, 11];
                    case 10: throw new Error("Unsupported operation kind: " + param.kind);
                    case 11:
                        params_1_1 = params_1.next();
                        return [3 /*break*/, 2];
                    case 12: return [3 /*break*/, 15];
                    case 13:
                        e_1_1 = _j.sent();
                        e_1 = { error: e_1_1 };
                        return [3 /*break*/, 15];
                    case 14:
                        try {
                            if (params_1_1 && !params_1_1.done && (_h = params_1.return)) _h.call(params_1);
                        }
                        finally { if (e_1) throw e_1.error; }
                        return [7 /*endfinally*/];
                    case 15: return [2 /*return*/, estimates];
                }
            });
        });
    };
    return NaiveEstimateProvider;
}());
exports.NaiveEstimateProvider = NaiveEstimateProvider;
//# sourceMappingURL=naive-estimate-provider.js.map