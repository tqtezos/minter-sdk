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
exports.ComparableToken = exports.Token = exports.TokenValidationError = void 0;
var TokenValidationError = /** @class */ (function () {
    function TokenValidationError(value, token, baseMessage) {
        this.value = value;
        this.token = token;
        this.name = 'ValidationError';
        var annot = this.token.annot();
        var annotText = annot ? "[" + annot + "] " : '';
        this.message = "" + annotText + baseMessage;
    }
    return TokenValidationError;
}());
exports.TokenValidationError = TokenValidationError;
var Token = /** @class */ (function () {
    function Token(val, idx, fac) {
        this.val = val;
        this.idx = idx;
        this.fac = fac;
        this.createToken = this.fac;
    }
    Token.prototype.typeWithoutAnnotations = function () {
        var removeArgsRec = function (val) {
            if (val.args) {
                return {
                    prim: val.prim,
                    args: val.args.map(function (x) { return removeArgsRec(x); }),
                };
            }
            else {
                return {
                    prim: val.prim,
                };
            }
        };
        return removeArgsRec(this.val);
    };
    Token.prototype.annot = function () {
        return (Array.isArray(this.val.annots) && this.val.annots.length > 0
            ? this.val.annots[0]
            : String(this.idx)).replace(/(%|\:)(_Liq_entry_)?/, '');
    };
    Token.prototype.hasAnnotations = function () {
        return Array.isArray(this.val.annots) && this.val.annots.length;
    };
    Token.prototype.ExtractSignature = function () {
        return [[this.ExtractSchema()]];
    };
    return Token;
}());
exports.Token = Token;
var ComparableToken = /** @class */ (function (_super) {
    __extends(ComparableToken, _super);
    function ComparableToken() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ComparableToken.prototype.compare = function (o1, o2) {
        if (o1 === o2) {
            return 0;
        }
        return o1 < o2 ? -1 : 1;
    };
    return ComparableToken;
}(Token));
exports.ComparableToken = ComparableToken;
//# sourceMappingURL=token.js.map