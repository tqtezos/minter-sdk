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
exports.ChainIDToken = exports.ChainIDValidationError = void 0;
var token_1 = require("./token");
var utils_1 = require("@taquito/utils");
var ChainIDValidationError = /** @class */ (function (_super) {
    __extends(ChainIDValidationError, _super);
    function ChainIDValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'ChainIDValidationError';
        return _this;
    }
    return ChainIDValidationError;
}(token_1.TokenValidationError));
exports.ChainIDValidationError = ChainIDValidationError;
var ChainIDToken = /** @class */ (function (_super) {
    __extends(ChainIDToken, _super);
    function ChainIDToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    ChainIDToken.prototype.isValid = function (value) {
        if (utils_1.validateChain(value) !== utils_1.ValidationResult.VALID) {
            return new ChainIDValidationError(value, this, 'ChainID is not valid');
        }
        return null;
    };
    ChainIDToken.prototype.Execute = function (val) {
        return val[Object.keys(val)[0]];
    };
    ChainIDToken.prototype.ExtractSchema = function () {
        return ChainIDToken.prim;
    };
    ChainIDToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    ChainIDToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    // tslint:disable-next-line: variable-name
    ChainIDToken.prototype.ToKey = function (_a) {
        var string = _a.string;
        return string;
    };
    ChainIDToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { string: val },
            type: { prim: ChainIDToken.prim },
        };
    };
    ChainIDToken.prim = 'chain_id';
    return ChainIDToken;
}(token_1.ComparableToken));
exports.ChainIDToken = ChainIDToken;
//# sourceMappingURL=chain-id.js.map