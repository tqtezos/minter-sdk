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
exports.LambdaToken = void 0;
var token_1 = require("./token");
var LambdaToken = /** @class */ (function (_super) {
    __extends(LambdaToken, _super);
    function LambdaToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    LambdaToken.prototype.Execute = function (val) {
        return val.string;
    };
    LambdaToken.prototype.Encode = function (args) {
        var val = args.pop();
        return val;
    };
    LambdaToken.prototype.EncodeObject = function (val) {
        return val;
    };
    LambdaToken.prototype.ExtractSchema = function () {
        var _a;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var rightToken = this.createToken(this.val.args[1], this.idx + 1);
        return _a = {},
            _a[LambdaToken.prim] = {
                parameters: leftToken.ExtractSchema(),
                returns: rightToken.ExtractSchema(),
            },
            _a;
    };
    LambdaToken.prim = 'lambda';
    return LambdaToken;
}(token_1.Token));
exports.LambdaToken = LambdaToken;
//# sourceMappingURL=lambda.js.map