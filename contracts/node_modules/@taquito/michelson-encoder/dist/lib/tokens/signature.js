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
exports.SignatureToken = exports.SignatureValidationError = void 0;
var token_1 = require("./token");
var utils_1 = require("@taquito/utils");
var SignatureValidationError = /** @class */ (function (_super) {
    __extends(SignatureValidationError, _super);
    function SignatureValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'SignatureValidationError';
        return _this;
    }
    return SignatureValidationError;
}(token_1.TokenValidationError));
exports.SignatureValidationError = SignatureValidationError;
var SignatureToken = /** @class */ (function (_super) {
    __extends(SignatureToken, _super);
    function SignatureToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    SignatureToken.prototype.Execute = function (val) {
        return val.string;
    };
    SignatureToken.prototype.isValid = function (value) {
        if (utils_1.validateSignature(value) !== utils_1.ValidationResult.VALID) {
            return new SignatureValidationError(value, this, 'Signature is not valid');
        }
        return null;
    };
    SignatureToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    SignatureToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    SignatureToken.prototype.ExtractSchema = function () {
        return SignatureToken.prim;
    };
    SignatureToken.prim = 'signature';
    return SignatureToken;
}(token_1.Token));
exports.SignatureToken = SignatureToken;
//# sourceMappingURL=signature.js.map