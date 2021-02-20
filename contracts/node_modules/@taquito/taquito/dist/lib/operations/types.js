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
Object.defineProperty(exports, "__esModule", { value: true });
exports.hasMetadataWithInternalOperationResult = exports.hasMetadataWithResult = exports.hasMetadata = exports.isSourceOp = exports.isOpRequireReveal = exports.isOpWithFee = exports.isKind = exports.findWithKind = exports.attachKind = exports.OpKind = void 0;
var rpc_1 = require("@taquito/rpc");
Object.defineProperty(exports, "OpKind", { enumerable: true, get: function () { return rpc_1.OpKind; } });
var attachKind = function (op, kind) {
    return __assign(__assign({}, op), { kind: kind });
};
exports.attachKind = attachKind;
var findWithKind = function (arr, kind) {
    if (Array.isArray(arr)) {
        var found = arr.find(function (op) { return op.kind === kind; });
        if (found && exports.isKind(found, kind)) {
            return found;
        }
    }
};
exports.findWithKind = findWithKind;
var isKind = function (op, kind) {
    return op.kind === kind;
};
exports.isKind = isKind;
var isOpWithFee = function (op) {
    return ['transaction', 'delegation', 'origination', 'reveal'].indexOf(op.kind) !== -1;
};
exports.isOpWithFee = isOpWithFee;
var isOpRequireReveal = function (op) {
    return ['transaction', 'delegation', 'origination'].indexOf(op.kind) !== -1;
};
exports.isOpRequireReveal = isOpRequireReveal;
var isSourceOp = function (op) {
    return ['transaction', 'delegation', 'origination', 'reveal', 'ballot'].indexOf(op.kind) !== -1;
};
exports.isSourceOp = isSourceOp;
var hasMetadata = function (op) {
    return 'metadata' in op;
};
exports.hasMetadata = hasMetadata;
var hasMetadataWithResult = function (op) {
    return exports.hasMetadata(op) && 'operation_result' in op.metadata;
};
exports.hasMetadataWithResult = hasMetadataWithResult;
var hasMetadataWithInternalOperationResult = function (op) {
    return exports.hasMetadata(op) && 'internal_operation_results' in op.metadata;
};
exports.hasMetadataWithInternalOperationResult = hasMetadataWithInternalOperationResult;
//# sourceMappingURL=types.js.map