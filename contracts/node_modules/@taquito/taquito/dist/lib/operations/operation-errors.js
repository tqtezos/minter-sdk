"use strict";
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
exports.flattenErrors = exports.flattenOperationResult = exports.TezosPreapplyFailureError = exports.TezosOperationError = void 0;
var types_1 = require("./types");
var isErrorWithMessage = function (error) {
    return 'with' in error;
};
var TezosOperationError = /** @class */ (function () {
    function TezosOperationError(errors) {
        this.errors = errors;
        this.name = 'TezosOperationError';
        // Last error is 'often' the one with more detail
        var lastError = errors[errors.length - 1];
        this.id = lastError.id;
        this.kind = lastError.kind;
        this.message = "(" + this.kind + ") " + this.id;
        if (isErrorWithMessage(lastError) && lastError.with.string) {
            this.message = lastError.with.string;
        }
    }
    return TezosOperationError;
}());
exports.TezosOperationError = TezosOperationError;
var TezosPreapplyFailureError = /** @class */ (function () {
    function TezosPreapplyFailureError(result) {
        this.result = result;
        this.name = 'TezosPreapplyFailureError';
        this.message = 'Preapply returned an unexpected result';
    }
    return TezosPreapplyFailureError;
}());
exports.TezosPreapplyFailureError = TezosPreapplyFailureError;
// Flatten all operation content results and internal operation results into a single array
// Some cases where we can have multiple operation results or internal operation results are:
// - When an operation includes a reveal operation
// - When an operation is made using the batch API
// - Smart contract call can contains internal operation results when they call other smart contract internally or originate contracts
var flattenOperationResult = function (response) {
    var results = Array.isArray(response) ? response : [response];
    var returnedResults = [];
    for (var i = 0; i < results.length; i++) {
        for (var j = 0; j < results[i].contents.length; j++) {
            var content = results[i].contents[j];
            if (types_1.hasMetadataWithResult(content)) {
                returnedResults.push(__assign({ fee: content.fee }, content.metadata.operation_result));
                if (Array.isArray(content.metadata.internal_operation_results)) {
                    content.metadata.internal_operation_results.forEach(function (x) { return returnedResults.push(x.result); });
                }
            }
        }
    }
    return returnedResults;
};
exports.flattenOperationResult = flattenOperationResult;
/***
 * @description Flatten all error from preapply response (including internal error)
 */
var flattenErrors = function (response, status) {
    var e_1, _a;
    if (status === void 0) { status = 'failed'; }
    var results = Array.isArray(response) ? response : [response];
    var errors = [];
    // Transaction that do not fail will be backtracked in case one failure occur
    for (var i = 0; i < results.length; i++) {
        for (var j = 0; j < results[i].contents.length; j++) {
            var content = results[i].contents[j];
            if (types_1.hasMetadata(content)) {
                if (types_1.hasMetadataWithResult(content) && content.metadata.operation_result.status === status) {
                    errors = errors.concat(content.metadata.operation_result.errors || []);
                }
                if (types_1.hasMetadataWithInternalOperationResult(content) &&
                    Array.isArray(content.metadata.internal_operation_results)) {
                    try {
                        for (var _b = (e_1 = void 0, __values(content.metadata.internal_operation_results)), _c = _b.next(); !_c.done; _c = _b.next()) {
                            var internalResult = _c.value;
                            if ('result' in internalResult && internalResult.result.status === status) {
                                errors = errors.concat(internalResult.result.errors || []);
                            }
                        }
                    }
                    catch (e_1_1) { e_1 = { error: e_1_1 }; }
                    finally {
                        try {
                            if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                        }
                        finally { if (e_1) throw e_1.error; }
                    }
                }
            }
        }
    }
    return errors;
};
exports.flattenErrors = flattenErrors;
//# sourceMappingURL=operation-errors.js.map