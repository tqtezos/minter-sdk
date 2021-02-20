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
exports.NatToken = exports.NatValidationError = void 0;
var token_1 = require("../token");
var bignumber_js_1 = require("bignumber.js");
var NatValidationError = /** @class */ (function (_super) {
    __extends(NatValidationError, _super);
    function NatValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'NatValidationError';
        return _this;
    }
    return NatValidationError;
}(token_1.TokenValidationError));
exports.NatValidationError = NatValidationError;
var NatToken = /** @class */ (function (_super) {
    __extends(NatToken, _super);
    function NatToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    NatToken.prototype.Execute = function (val) {
        return new bignumber_js_1.default(val[Object.keys(val)[0]]);
    };
    NatToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    NatToken.prototype.isValid = function (val) {
        var bigNumber = new bignumber_js_1.default(val);
        if (bigNumber.isNaN()) {
            return new NatValidationError(val, this, "Value is not a number: " + val);
        }
        else if (bigNumber.isNegative()) {
            return new NatValidationError(val, this, "Value cannot be negative: " + val);
        }
        else {
            return null;
        }
    };
    NatToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    NatToken.prototype.ExtractSchema = function () {
        return NatToken.prim;
    };
    NatToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { int: val },
            type: { prim: NatToken.prim },
        };
    };
    NatToken.prototype.ToKey = function (_a) {
        var int = _a.int;
        return int;
    };
    NatToken.prim = 'nat';
    return NatToken;
}(token_1.ComparableToken));
exports.NatToken = NatToken;
//# sourceMappingURL=nat.js.map