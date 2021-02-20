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
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __values = (this && this.__values) || function(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
};
var __read = (this && this.__read) || function (o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
};
var __spread = (this && this.__spread) || function () {
    for (var ar = [], i = 0; i < arguments.length; i++) ar = ar.concat(__read(arguments[i]));
    return ar;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.PairToken = void 0;
var token_1 = require("./token");
var or_1 = require("./or");
var PairToken = /** @class */ (function (_super) {
    __extends(PairToken, _super);
    function PairToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    PairToken.prototype.Encode = function (args) {
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof PairToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        return {
            prim: 'Pair',
            args: [leftToken.Encode(args), rightToken.Encode(args)],
        };
    };
    PairToken.prototype.ExtractSignature = function () {
        var e_1, _a, e_2, _b;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof or_1.OrToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var newSig = [];
        try {
            for (var _c = __values(leftToken.ExtractSignature()), _d = _c.next(); !_d.done; _d = _c.next()) {
                var leftSig = _d.value;
                try {
                    for (var _e = (e_2 = void 0, __values(rightToken.ExtractSignature())), _f = _e.next(); !_f.done; _f = _e.next()) {
                        var rightSig = _f.value;
                        newSig.push(__spread(leftSig, rightSig));
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return newSig;
    };
    PairToken.prototype.ToBigMapKey = function (val) {
        return {
            key: this.EncodeObject(val),
            type: this.typeWithoutAnnotations(),
        };
    };
    PairToken.prototype.ToKey = function (val) {
        return this.Execute(val);
    };
    PairToken.prototype.EncodeObject = function (args) {
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof PairToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var leftValue;
        if (leftToken instanceof PairToken && !leftToken.hasAnnotations()) {
            leftValue = args;
        }
        else {
            leftValue = args[leftToken.annot()];
        }
        var rightValue;
        if (rightToken instanceof PairToken && !rightToken.hasAnnotations()) {
            rightValue = args;
        }
        else {
            rightValue = args[rightToken.annot()];
        }
        return {
            prim: 'Pair',
            args: [leftToken.EncodeObject(leftValue), rightToken.EncodeObject(rightValue)],
        };
    };
    PairToken.prototype.traversal = function (getLeftValue, getRightValue) {
        var _a, _b;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        var leftValue;
        if (leftToken instanceof PairToken && !leftToken.hasAnnotations()) {
            leftValue = getLeftValue(leftToken);
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        else {
            leftValue = (_a = {}, _a[leftToken.annot()] = getLeftValue(leftToken), _a);
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var rightValue;
        if (rightToken instanceof PairToken && !rightToken.hasAnnotations()) {
            rightValue = getRightValue(rightToken);
        }
        else {
            rightValue = (_b = {}, _b[rightToken.annot()] = getRightValue(rightToken), _b);
        }
        var res = __assign(__assign({}, leftValue), rightValue);
        return res;
    };
    PairToken.prototype.Execute = function (val, semantics) {
        return this.traversal(function (leftToken) { return leftToken.Execute(val.args[0], semantics); }, function (rightToken) { return rightToken.Execute(val.args[1], semantics); });
    };
    PairToken.prototype.ExtractSchema = function () {
        return this.traversal(function (leftToken) { return leftToken.ExtractSchema(); }, function (rightToken) { return rightToken.ExtractSchema(); });
    };
    PairToken.prototype.compare = function (val1, val2) {
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof PairToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var getValue = function (token, args) {
            if (token instanceof PairToken && !token.hasAnnotations()) {
                return args;
            }
            else {
                return args[token.annot()];
            }
        };
        if (leftToken instanceof token_1.ComparableToken && rightToken instanceof token_1.ComparableToken) {
            var result = leftToken.compare(getValue(leftToken, val1), getValue(leftToken, val2));
            if (result === 0) {
                return rightToken.compare(getValue(rightToken, val1), getValue(rightToken, val2));
            }
            return result;
        }
        throw new Error('Not a comparable pair');
    };
    PairToken.prim = 'pair';
    return PairToken;
}(token_1.ComparableToken));
exports.PairToken = PairToken;
//# sourceMappingURL=pair.js.map