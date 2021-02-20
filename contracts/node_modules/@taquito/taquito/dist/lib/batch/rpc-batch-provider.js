"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
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
exports.RPCBatchProvider = exports.OperationBatch = exports.BATCH_KINDS = void 0;
var prepare_1 = require("../contract/prepare");
var batch_operation_1 = require("../operations/batch-operation");
var operation_emitter_1 = require("../operations/operation-emitter");
var types_1 = require("../operations/types");
var rpc_1 = require("@taquito/rpc");
exports.BATCH_KINDS = [
    rpc_1.OpKind.ACTIVATION,
    rpc_1.OpKind.ORIGINATION,
    rpc_1.OpKind.TRANSACTION,
    rpc_1.OpKind.DELEGATION,
];
var OperationBatch = /** @class */ (function (_super) {
    __extends(OperationBatch, _super);
    function OperationBatch(context, estimator) {
        var _this = _super.call(this, context) || this;
        _this.estimator = estimator;
        _this.operations = [];
        return _this;
    }
    /**
     *
     * @description Add a transaction operation to the batch
     *
     * @param params Transfer operation parameter
     */
    OperationBatch.prototype.withTransfer = function (params) {
        this.operations.push(__assign({ kind: rpc_1.OpKind.TRANSACTION }, params));
        return this;
    };
    /**
     *
     * @description Add a transaction operation to the batch
     *
     * @param params Transfer operation parameter
     */
    OperationBatch.prototype.withContractCall = function (params) {
        return this.withTransfer(params.toTransferParams());
    };
    /**
     *
     * @description Add a delegation operation to the batch
     *
     * @param params Delegation operation parameter
     */
    OperationBatch.prototype.withDelegation = function (params) {
        this.operations.push(__assign({ kind: rpc_1.OpKind.DELEGATION }, params));
        return this;
    };
    /**
     *
     * @description Add an activation operation to the batch
     *
     * @param params Activation operation parameter
     */
    OperationBatch.prototype.withActivation = function (_a) {
        var pkh = _a.pkh, secret = _a.secret;
        this.operations.push({ kind: rpc_1.OpKind.ACTIVATION, pkh: pkh, secret: secret });
        return this;
    };
    /**
     *
     * @description Add an origination operation to the batch
     *
     * @param params Origination operation parameter
     */
    OperationBatch.prototype.withOrigination = function (params) {
        this.operations.push(__assign({ kind: rpc_1.OpKind.ORIGINATION }, params));
        return this;
    };
    OperationBatch.prototype.getRPCOp = function (param) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (param.kind) {
                    case rpc_1.OpKind.TRANSACTION:
                        return [2 /*return*/, prepare_1.createTransferOperation(__assign({}, param))];
                    case rpc_1.OpKind.ORIGINATION:
                        return [2 /*return*/, prepare_1.createOriginationOperation(__assign({}, param))];
                    case rpc_1.OpKind.DELEGATION:
                        return [2 /*return*/, prepare_1.createSetDelegateOperation(__assign({}, param))];
                    case rpc_1.OpKind.ACTIVATION:
                        return [2 /*return*/, __assign({}, param)];
                    default:
                        throw new Error("Unsupported operation kind: " + param.kind);
                }
                return [2 /*return*/];
            });
        });
    };
    /**
     *
     * @description Add a group operation to the batch. Operation will be applied in the order they are in the params array
     *
     * @param params Operations parameter
     */
    OperationBatch.prototype.with = function (params) {
        var e_1, _a;
        try {
            for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                var param = params_1_1.value;
                switch (param.kind) {
                    case rpc_1.OpKind.TRANSACTION:
                        this.withTransfer(param);
                        break;
                    case rpc_1.OpKind.ORIGINATION:
                        this.withOrigination(param);
                        break;
                    case rpc_1.OpKind.DELEGATION:
                        this.withDelegation(param);
                        break;
                    case rpc_1.OpKind.ACTIVATION:
                        this.withActivation(param);
                        break;
                    default:
                        throw new Error("Unsupported operation kind: " + param.kind);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (params_1_1 && !params_1_1.done && (_a = params_1.return)) _a.call(params_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return this;
    };
    /**
     *
     * @description Forge and Inject the operation batch
     *
     * @param params Optionally specify the source of the operation
     */
    OperationBatch.prototype.send = function (params) {
        return __awaiter(this, void 0, void 0, function () {
            var estimates, ops, i, _a, _b, op, estimated, _c, _d, e_2_1, source, _e, opBytes, _f, hash, context, forgedBytes, opResponse;
            var e_2, _g;
            var _this = this;
            return __generator(this, function (_h) {
                switch (_h.label) {
                    case 0: return [4 /*yield*/, this.estimator.batch(this.operations)];
                    case 1:
                        estimates = _h.sent();
                        ops = [];
                        i = 0;
                        _h.label = 2;
                    case 2:
                        _h.trys.push([2, 10, 11, 12]);
                        _a = __values(this.operations), _b = _a.next();
                        _h.label = 3;
                    case 3:
                        if (!!_b.done) return [3 /*break*/, 9];
                        op = _b.value;
                        if (!types_1.isOpWithFee(op)) return [3 /*break*/, 6];
                        return [4 /*yield*/, this.estimate(op, function () { return __awaiter(_this, void 0, void 0, function () { return __generator(this, function (_a) {
                                return [2 /*return*/, estimates[i]];
                            }); }); })];
                    case 4:
                        estimated = _h.sent();
                        _d = (_c = ops).push;
                        return [4 /*yield*/, this.getRPCOp(__assign(__assign({}, op), estimated))];
                    case 5:
                        _d.apply(_c, [_h.sent()]);
                        return [3 /*break*/, 7];
                    case 6:
                        ops.push(__assign({}, op));
                        _h.label = 7;
                    case 7:
                        i++;
                        _h.label = 8;
                    case 8:
                        _b = _a.next();
                        return [3 /*break*/, 3];
                    case 9: return [3 /*break*/, 12];
                    case 10:
                        e_2_1 = _h.sent();
                        e_2 = { error: e_2_1 };
                        return [3 /*break*/, 12];
                    case 11:
                        try {
                            if (_b && !_b.done && (_g = _a.return)) _g.call(_a);
                        }
                        finally { if (e_2) throw e_2.error; }
                        return [7 /*endfinally*/];
                    case 12:
                        _e = (params && params.source);
                        if (_e) return [3 /*break*/, 14];
                        return [4 /*yield*/, this.signer.publicKeyHash()];
                    case 13:
                        _e = (_h.sent());
                        _h.label = 14;
                    case 14:
                        source = _e;
                        return [4 /*yield*/, this.prepareAndForge({
                                operation: ops,
                                source: source,
                            })];
                    case 15:
                        opBytes = _h.sent();
                        return [4 /*yield*/, this.signAndInject(opBytes)];
                    case 16:
                        _f = _h.sent(), hash = _f.hash, context = _f.context, forgedBytes = _f.forgedBytes, opResponse = _f.opResponse;
                        return [2 /*return*/, new batch_operation_1.BatchOperation(hash, ops, source, forgedBytes, opResponse, context)];
                }
            });
        });
    };
    return OperationBatch;
}(operation_emitter_1.OperationEmitter));
exports.OperationBatch = OperationBatch;
var RPCBatchProvider = /** @class */ (function () {
    function RPCBatchProvider(context, estimator) {
        this.context = context;
        this.estimator = estimator;
    }
    /***
     *
     * @description Batch a group of operation together. Operations will be applied in the order in which they are added to the batch
     *
     * @param params List of operation to batch together
     */
    RPCBatchProvider.prototype.batch = function (params) {
        var batch = new OperationBatch(this.context, this.estimator);
        if (Array.isArray(params)) {
            batch.with(params);
        }
        return batch;
    };
    return RPCBatchProvider;
}());
exports.RPCBatchProvider = RPCBatchProvider;
//# sourceMappingURL=rpc-batch-provider.js.map