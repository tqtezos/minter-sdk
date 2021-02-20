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
exports.TimestampToken = void 0;
var token_1 = require("../token");
var TimestampToken = /** @class */ (function (_super) {
    __extends(TimestampToken, _super);
    function TimestampToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    TimestampToken.prototype.Execute = function (val) {
        if (val.string) {
            return new Date(val.string).toISOString();
        }
        else if (val.int) {
            return new Date(Number(val.int) * 1000).toISOString();
        }
    };
    TimestampToken.prototype.Encode = function (args) {
        var val = args.pop();
        return { string: val };
    };
    TimestampToken.prototype.EncodeObject = function (val) {
        return { string: val };
    };
    TimestampToken.prototype.ExtractSchema = function () {
        return TimestampToken.prim;
    };
    // tslint:disable-next-line: variable-name
    TimestampToken.prototype.ToKey = function (_a) {
        var string = _a.string;
        return string;
    };
    TimestampToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { string: val },
            type: { prim: TimestampToken.prim },
        };
    };
    TimestampToken.prim = 'timestamp';
    return TimestampToken;
}(token_1.ComparableToken));
exports.TimestampToken = TimestampToken;
//# sourceMappingURL=timestamp.js.map