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
exports.BigMapToken = exports.BigMapValidationError = void 0;
var michelson_map_1 = require("../michelson-map");
var token_1 = require("./token");
var BigMapValidationError = /** @class */ (function (_super) {
    __extends(BigMapValidationError, _super);
    function BigMapValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'BigMapValidationError';
        return _this;
    }
    return BigMapValidationError;
}(token_1.TokenValidationError));
exports.BigMapValidationError = BigMapValidationError;
var BigMapToken = /** @class */ (function (_super) {
    __extends(BigMapToken, _super);
    function BigMapToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    Object.defineProperty(BigMapToken.prototype, "ValueSchema", {
        get: function () {
            return this.createToken(this.val.args[1], 0);
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(BigMapToken.prototype, "KeySchema", {
        get: function () {
            return this.createToken(this.val.args[0], 0);
        },
        enumerable: false,
        configurable: true
    });
    BigMapToken.prototype.ExtractSchema = function () {
        var _a;
        return _a = {},
            _a[this.KeySchema.ExtractSchema()] = this.ValueSchema.ExtractSchema(),
            _a;
    };
    BigMapToken.prototype.isValid = function (value) {
        if (michelson_map_1.MichelsonMap.isMichelsonMap(value)) {
            return null;
        }
        return new BigMapValidationError(value, this, 'Value must be a MichelsonMap');
    };
    BigMapToken.prototype.Encode = function (args) {
        var _this = this;
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return Array.from(val.keys())
            .sort(function (a, b) { return _this.KeySchema.compare(a, b); })
            .map(function (key) {
            return {
                prim: 'Elt',
                args: [_this.KeySchema.EncodeObject(key), _this.ValueSchema.EncodeObject(val.get(key))],
            };
        });
    };
    BigMapToken.prototype.EncodeObject = function (args) {
        var _this = this;
        var val = args;
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return Array.from(val.keys())
            .sort(function (a, b) { return _this.KeySchema.compare(a, b); })
            .map(function (key) {
            return {
                prim: 'Elt',
                args: [_this.KeySchema.EncodeObject(key), _this.ValueSchema.EncodeObject(val.get(key))],
            };
        });
    };
    BigMapToken.prototype.Execute = function (val, semantic) {
        var _this = this;
        if (semantic && semantic[BigMapToken.prim]) {
            return semantic[BigMapToken.prim](val, this.val);
        }
        if (Array.isArray(val)) {
            // Athens is returning an empty array for big map in storage
            // Internal: In taquito v5 it is still used to decode big map diff (as if they were a regular map)
            var map_1 = new michelson_map_1.MichelsonMap(this.val);
            val.forEach(function (current) {
                map_1.set(_this.KeySchema.ToKey(current.args[0]), _this.ValueSchema.Execute(current.args[1]));
            });
            return map_1;
        }
        else if ('int' in val) {
            // Babylon is returning an int with the big map id in contract storage
            return val.int;
        }
        else {
            // Unknown case
            throw new Error("Big map is expecting either an array (Athens) or an object with an int property (Babylon). Got " + JSON.stringify(val));
        }
    };
    BigMapToken.prim = 'big_map';
    return BigMapToken;
}(token_1.Token));
exports.BigMapToken = BigMapToken;
//# sourceMappingURL=bigmap.js.map