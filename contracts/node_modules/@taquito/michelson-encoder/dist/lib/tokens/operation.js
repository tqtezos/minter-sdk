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
Object.defineProperty(exports, "__esModule", { value: true });
exports.OperationToken = void 0;
var token_1 = require("./token");
var OperationToken = /** @class */ (function (_super) {
    __extends(OperationToken, _super);
    function OperationToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    OperationToken.prototype.Execute = function (val) {
        return val.string;
    };
    OperationToken.prototype.Encode = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        var val = args.pop();
        return { string: val };
    };
    OperationToken.prototype.EncodeObject = function (val) {
        return { string: val };
    };
    OperationToken.prototype.ExtractSchema = function () {
        return OperationToken.prim;
    };
    OperationToken.prim = 'operation';
    return OperationToken;
}(token_1.Token));
exports.OperationToken = OperationToken;
//# sourceMappingURL=operation.js.map