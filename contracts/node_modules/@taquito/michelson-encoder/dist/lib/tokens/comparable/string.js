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
exports.StringToken = void 0;
var token_1 = require("../token");
var StringToken = /** @class */ (function (_super) {
    __extends(StringToken, _super);
    function StringToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    StringToken.prototype.Execute = function (val) {
        return val[Object.keys(val)[0]];
    };
    StringToken.prototype.ExtractSchema = function () {
        return StringToken.prim;
    };
    StringToken.prototype.Encode = function (args) {
        var val = args.pop();
        return { string: val };
    };
    StringToken.prototype.EncodeObject = function (val) {
        return { string: val };
    };
    // tslint:disable-next-line: variable-name
    StringToken.prototype.ToKey = function (_a) {
        var string = _a.string;
        return string;
    };
    StringToken.prototype.ToBigMapKey = function (val) {
        return {
            key: { string: val },
            type: { prim: StringToken.prim },
        };
    };
    StringToken.prim = 'string';
    return StringToken;
}(token_1.ComparableToken));
exports.StringToken = StringToken;
//# sourceMappingURL=string.js.map