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
Object.defineProperty(exports, "__esModule", { value: true });
exports.OrToken = void 0;
var token_1 = require("./token");
var OrToken = /** @class */ (function (_super) {
    __extends(OrToken, _super);
    function OrToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    OrToken.prototype.Encode = function (args) {
        var label = args[args.length - 1];
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof OrToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        if (String(leftToken.annot()) === String(label) && !(leftToken instanceof OrToken)) {
            args.pop();
            return { prim: 'Left', args: [leftToken.Encode(args)] };
        }
        else if (String(rightToken.annot()) === String(label) && !(rightToken instanceof OrToken)) {
            args.pop();
            return { prim: 'Right', args: [rightToken.Encode(args)] };
        }
        else {
            if (leftToken instanceof OrToken) {
                var val = leftToken.Encode(args);
                if (val) {
                    return { prim: 'Left', args: [val] };
                }
            }
            if (rightToken instanceof OrToken) {
                var val = rightToken.Encode(args);
                if (val) {
                    return { prim: 'Right', args: [val] };
                }
            }
            return null;
        }
    };
    OrToken.prototype.ExtractSignature = function () {
        var e_1, _a, e_2, _b;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof OrToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var newSig = [];
        if (leftToken instanceof OrToken) {
            newSig.push.apply(newSig, __spread(leftToken.ExtractSignature()));
        }
        else {
            try {
                for (var _c = __values(leftToken.ExtractSignature()), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var sig = _d.value;
                    newSig.push(__spread([leftToken.annot()], sig));
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        if (rightToken instanceof OrToken) {
            newSig.push.apply(newSig, __spread(rightToken.ExtractSignature()));
        }
        else {
            try {
                for (var _e = __values(rightToken.ExtractSignature()), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var sig = _f.value;
                    newSig.push(__spread([rightToken.annot()], sig));
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
        return newSig;
    };
    OrToken.prototype.EncodeObject = function (args) {
        var label = Object.keys(args)[0];
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof OrToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        if (String(leftToken.annot()) === String(label) && !(leftToken instanceof OrToken)) {
            return { prim: 'Left', args: [leftToken.EncodeObject(args[label])] };
        }
        else if (String(rightToken.annot()) === String(label) && !(rightToken instanceof OrToken)) {
            return { prim: 'Right', args: [rightToken.EncodeObject(args[label])] };
        }
        else {
            if (leftToken instanceof OrToken) {
                var val = leftToken.EncodeObject(args);
                if (val) {
                    return { prim: 'Left', args: [val] };
                }
            }
            if (rightToken instanceof OrToken) {
                var val = rightToken.EncodeObject(args);
                if (val) {
                    return { prim: 'Right', args: [val] };
                }
            }
            return null;
        }
    };
    OrToken.prototype.Execute = function (val, semantics) {
        var _a;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        if (leftToken instanceof OrToken) {
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        if (val.prim === 'Right') {
            return rightToken.Execute(val.args[0], semantics);
        }
        else if (val.prim === 'Left') {
            return _a = {},
                _a[leftToken.annot()] = leftToken.Execute(val.args[0], semantics),
                _a;
        }
        else {
            throw new Error("Was expecting Left or Right prim but got: " + val.prim);
        }
    };
    OrToken.prototype.traversal = function (getLeftValue, getRightValue, concat) {
        var _a, _b;
        var leftToken = this.createToken(this.val.args[0], this.idx);
        var keyCount = 1;
        var leftValue;
        if (leftToken instanceof OrToken && !leftToken.hasAnnotations()) {
            leftValue = getLeftValue(leftToken);
            keyCount = Object.keys(leftToken.ExtractSchema()).length;
        }
        else {
            leftValue = (_a = {}, _a[leftToken.annot()] = getLeftValue(leftToken), _a);
        }
        var rightToken = this.createToken(this.val.args[1], this.idx + keyCount);
        var rightValue;
        if (rightToken instanceof OrToken && !rightToken.hasAnnotations()) {
            rightValue = getRightValue(rightToken);
        }
        else {
            rightValue = (_b = {}, _b[rightToken.annot()] = getRightValue(rightToken), _b);
        }
        var res = concat(leftValue, rightValue);
        return res;
    };
    OrToken.prototype.ExtractSchema = function () {
        return this.traversal(function (leftToken) { return leftToken.ExtractSchema(); }, function (rightToken) { return rightToken.ExtractSchema(); }, function (leftValue, rightValue) { return (__assign(__assign({}, leftValue), rightValue)); });
    };
    OrToken.prim = 'or';
    return OrToken;
}(token_1.Token));
exports.OrToken = OrToken;
//# sourceMappingURL=or.js.map