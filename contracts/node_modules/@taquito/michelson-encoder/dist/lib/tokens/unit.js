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
exports.UnitToken = void 0;
var token_1 = require("./token");
var taquito_michelson_encoder_1 = require("../taquito-michelson-encoder");
var UnitToken = /** @class */ (function (_super) {
    __extends(UnitToken, _super);
    function UnitToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    UnitToken.prototype.Encode = function (args) {
        args.pop();
        return { prim: 'Unit' };
    };
    UnitToken.prototype.EncodeObject = function (_val) {
        return { prim: 'Unit' };
    };
    UnitToken.prototype.Execute = function () {
        return taquito_michelson_encoder_1.UnitValue;
    };
    UnitToken.prototype.ExtractSchema = function () {
        return UnitToken.prim;
    };
    UnitToken.prim = 'unit';
    return UnitToken;
}(token_1.Token));
exports.UnitToken = UnitToken;
//# sourceMappingURL=unit.js.map