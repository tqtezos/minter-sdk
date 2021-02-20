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
exports.MapToken = exports.MapValidationError = void 0;
var michelson_map_1 = require("../michelson-map");
var token_1 = require("./token");
var MapValidationError = /** @class */ (function (_super) {
    __extends(MapValidationError, _super);
    function MapValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'MapValidationError';
        return _this;
    }
    return MapValidationError;
}(token_1.TokenValidationError));
exports.MapValidationError = MapValidationError;
var MapToken = /** @class */ (function (_super) {
    __extends(MapToken, _super);
    function MapToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    Object.defineProperty(MapToken.prototype, "ValueSchema", {
        get: function () {
            return this.createToken(this.val.args[1], 0);
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(MapToken.prototype, "KeySchema", {
        get: function () {
            return this.createToken(this.val.args[0], 0);
        },
        enumerable: false,
        configurable: true
    });
    MapToken.prototype.isValid = function (value) {
        if (michelson_map_1.MichelsonMap.isMichelsonMap(value)) {
            return null;
        }
        return new MapValidationError(value, this, 'Value must be a MichelsonMap');
    };
    MapToken.prototype.Execute = function (val, semantics) {
        var _this = this;
        var map = new michelson_map_1.MichelsonMap(this.val);
        val.forEach(function (current) {
            map.set(_this.KeySchema.ToKey(current.args[0]), _this.ValueSchema.Execute(current.args[1], semantics));
        });
        return map;
    };
    MapToken.prototype.Encode = function (args) {
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
    MapToken.prototype.EncodeObject = function (args) {
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
    MapToken.prototype.ExtractSchema = function () {
        return {
            map: {
                key: this.KeySchema.ExtractSchema(),
                value: this.ValueSchema.ExtractSchema(),
            },
        };
    };
    MapToken.prim = 'map';
    return MapToken;
}(token_1.Token));
exports.MapToken = MapToken;
//# sourceMappingURL=map.js.map