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
exports.ContractToken = exports.ContractValidationError = void 0;
var utils_1 = require("@taquito/utils");
var token_1 = require("./token");
var ContractValidationError = /** @class */ (function (_super) {
    __extends(ContractValidationError, _super);
    function ContractValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'ContractValidationError';
        return _this;
    }
    return ContractValidationError;
}(token_1.TokenValidationError));
exports.ContractValidationError = ContractValidationError;
var ContractToken = /** @class */ (function (_super) {
    __extends(ContractToken, _super);
    function ContractToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    ContractToken.prototype.isValid = function (value) {
        // tz1,tz2 and tz3 seems to be valid contract values (for Unit contract)
        if (utils_1.validateAddress(value) !== utils_1.ValidationResult.VALID) {
            return new ContractValidationError(value, this, 'Contract address is not valid');
        }
        return null;
    };
    ContractToken.prototype.Execute = function (val) {
        if (val.string) {
            return val.string;
        }
        return utils_1.encodePubKey(val.bytes);
    };
    ContractToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    ContractToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    ContractToken.prototype.ExtractSchema = function () {
        return ContractToken.prim;
    };
    ContractToken.prim = 'contract';
    return ContractToken;
}(token_1.Token));
exports.ContractToken = ContractToken;
//# sourceMappingURL=contract.js.map