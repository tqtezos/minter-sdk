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
exports.KeyToken = exports.KeyValidationError = void 0;
var token_1 = require("./token");
var utils_1 = require("@taquito/utils");
var KeyValidationError = /** @class */ (function (_super) {
    __extends(KeyValidationError, _super);
    function KeyValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'KeyValidationError';
        return _this;
    }
    return KeyValidationError;
}(token_1.TokenValidationError));
exports.KeyValidationError = KeyValidationError;
var KeyToken = /** @class */ (function (_super) {
    __extends(KeyToken, _super);
    function KeyToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    KeyToken.prototype.Execute = function (val) {
        if (val.string) {
            return val.string;
        }
        return utils_1.encodeKey(val.bytes);
    };
    KeyToken.prototype.isValid = function (value) {
        if (utils_1.validatePublicKey(value) !== utils_1.ValidationResult.VALID) {
            return new KeyValidationError(value, this, 'Key is not valid');
        }
        return null;
    };
    KeyToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    KeyToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    KeyToken.prototype.ExtractSchema = function () {
        return KeyToken.prim;
    };
    KeyToken.prim = 'key';
    return KeyToken;
}(token_1.Token));
exports.KeyToken = KeyToken;
//# sourceMappingURL=key.js.map