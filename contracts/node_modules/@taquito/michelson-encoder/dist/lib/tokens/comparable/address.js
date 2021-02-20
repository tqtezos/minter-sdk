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
exports.AddressToken = exports.AddressValidationError = void 0;
var token_1 = require("../token");
var utils_1 = require("@taquito/utils");
var AddressValidationError = /** @class */ (function (_super) {
    __extends(AddressValidationError, _super);
    function AddressValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'AddressValidationError';
        return _this;
    }
    return AddressValidationError;
}(token_1.TokenValidationError));
exports.AddressValidationError = AddressValidationError;
var AddressToken = /** @class */ (function (_super) {
    __extends(AddressToken, _super);
    function AddressToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    AddressToken.prototype.ToBigMapKey = function (val) {
        var decoded = utils_1.b58decode(val);
        return {
            key: { bytes: decoded },
            type: { prim: 'bytes' },
        };
    };
    AddressToken.prototype.isValid = function (value) {
        if (utils_1.validateAddress(value) !== utils_1.ValidationResult.VALID) {
            return new AddressValidationError(value, this, "Address is not valid: " + value);
        }
        return null;
    };
    AddressToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    AddressToken.prototype.EncodeObject = function (val) {
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return { string: val };
    };
    // tslint:disable-next-line: variable-name
    AddressToken.prototype.Execute = function (val) {
        if (val.string) {
            return val.string;
        }
        return utils_1.encodePubKey(val.bytes);
    };
    AddressToken.prototype.ExtractSchema = function () {
        return AddressToken.prim;
    };
    // tslint:disable-next-line: variable-name
    AddressToken.prototype.ToKey = function (_a) {
        var bytes = _a.bytes, string = _a.string;
        if (string) {
            return string;
        }
        return utils_1.encodePubKey(bytes);
    };
    AddressToken.prototype.compare = function (address1, address2) {
        var isImplicit = function (address) {
            return address.startsWith('tz');
        };
        if (isImplicit(address1) && isImplicit(address2)) {
            return _super.prototype.compare.call(this, address1, address2);
        }
        else if (isImplicit(address1)) {
            return -1;
        }
        else if (isImplicit(address2)) {
            return 1;
        }
        else {
            return _super.prototype.compare.call(this, address1, address2);
        }
    };
    AddressToken.prim = 'address';
    return AddressToken;
}(token_1.ComparableToken));
exports.AddressToken = AddressToken;
//# sourceMappingURL=address.js.map