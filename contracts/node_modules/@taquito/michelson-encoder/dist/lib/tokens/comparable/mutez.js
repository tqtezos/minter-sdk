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
exports.MutezToken = exports.MutezValidationError = void 0;
var token_1 = require("../token");
var bignumber_js_1 = require("bignumber.js");
var MutezValidationError = /** @class */ (function (_super) {
    __extends(MutezValidationError, _super);
    function MutezValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'MutezValidationError';
        return _this;
    }
    return MutezValidationError;
}(token_1.TokenValidationError));
exports.MutezValidationError = MutezValidationError;
var MutezToken = /** @class */ (function (_super) {
    __extends(MutezToken, _super);
    function MutezToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    MutezToken.prototype.Execute = function (val) {
        return new bignumber_js_1.default(val[Object.keys(val)[0]]);
    };
    MutezToken.prototype.ExtractSchema = function () {
        return MutezToken.prim;
    };
    MutezToken.prototype.isValid = function (val) {
        var bigNumber = new bignumber_js_1.default(val);
        if (bigNumber.isNaN()) {
            return new MutezValidationError(val, this, "Value is not a number: " + val);
        }
        else {
            return null;
        }
    };
    MutezToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    MutezToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { int: String(val).toString() };
    };
    MutezToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { int: val },
            type: { prim: MutezToken.prim },
        };
    };
    MutezToken.prototype.ToKey = function (_a) {
        var int = _a.int;
        return int;
    };
    MutezToken.prim = 'mutez';
    return MutezToken;
}(token_1.ComparableToken));
exports.MutezToken = MutezToken;
//# sourceMappingURL=mutez.js.map