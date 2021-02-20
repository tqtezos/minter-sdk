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
exports.BoolToken = void 0;
var token_1 = require("../token");
var BoolToken = /** @class */ (function (_super) {
    __extends(BoolToken, _super);
    function BoolToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    BoolToken.prototype.Execute = function (val) {
        return String(val.prim).toLowerCase() === 'true' ? true : false;
    };
    BoolToken.prototype.Encode = function (args) {
        var val = args.pop();
        return { prim: val ? 'True' : 'False' };
    };
    BoolToken.prototype.EncodeObject = function (val) {
        return { prim: val ? 'True' : 'False' };
    };
    BoolToken.prototype.ExtractSchema = function () {
        return BoolToken.prim;
    };
    BoolToken.prototype.ToBigMapKey = function (val) {
        return {
            key: this.EncodeObject(val),
            type: { prim: BoolToken.prim },
        };
    };
    BoolToken.prototype.ToKey = function (val) {
        return this.EncodeObject(val);
    };
    BoolToken.prototype.compare = function (val1, val2) {
        if ((val1 && val2) || (!val1 && !val2)) {
            return 0;
        }
        else if (val1) {
            return 1;
        }
        else {
            return -1;
        }
    };
    BoolToken.prim = 'bool';
    return BoolToken;
}(token_1.ComparableToken));
exports.BoolToken = BoolToken;
//# sourceMappingURL=bool.js.map