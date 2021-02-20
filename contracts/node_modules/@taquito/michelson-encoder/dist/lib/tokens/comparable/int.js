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
exports.IntToken = exports.IntValidationError = void 0;
var token_1 = require("../token");
var bignumber_js_1 = require("bignumber.js");
var IntValidationError = /** @class */ (function (_super) {
    __extends(IntValidationError, _super);
    function IntValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'IntValidationError';
        return _this;
    }
    return IntValidationError;
}(token_1.TokenValidationError));
exports.IntValidationError = IntValidationError;
var IntToken = /** @class */ (function (_super) {
    __extends(IntToken, _super);
    function IntToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    IntToken.prototype.Execute = function (val) {
        return new bignumber_js_1.default(val[Object.keys(val)[0]]);
    };
    IntToken.prototype.ExtractSchema = function () {
        return IntToken.prim;
    };
    IntToken.prototype.isValid = function (val) {
        var bigNumber = new bignumber_js_1.default(val);
        if (bigNumber.isNaN()) {
            return new IntValidationError(val, this, "Value is not a number: " + val);
        }
        else {
            return null;
        }
    };
    IntToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    IntToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    IntToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { int: val },
            type: { prim: IntToken.prim },
        };
    };
    IntToken.prototype.ToKey = function (_a) {
        var int = _a.int;
        return int;
    };
    IntToken.prim = 'int';
    return IntToken;
}(token_1.ComparableToken));
exports.IntToken = IntToken;
//# sourceMappingURL=int.js.map