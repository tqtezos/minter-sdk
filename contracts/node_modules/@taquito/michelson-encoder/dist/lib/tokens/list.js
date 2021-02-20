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
exports.ListToken = exports.ListValidationError = void 0;
var token_1 = require("./token");
var ListValidationError = /** @class */ (function (_super) {
    __extends(ListValidationError, _super);
    function ListValidationError(value, token, message) {
        var _this = _super.call(this, value, token, message) || this;
        _this.value = value;
        _this.token = token;
        _this.name = 'ListValidationError';
        return _this;
    }
    return ListValidationError;
}(token_1.TokenValidationError));
exports.ListValidationError = ListValidationError;
var ListToken = /** @class */ (function (_super) {
    __extends(ListToken, _super);
    function ListToken(val, idx, fac) {
        var _this = _super.call(this, val, idx, fac) || this;
        _this.val = val;
        _this.idx = idx;
        _this.fac = fac;
        return _this;
    }
    ListToken.prototype.isValid = function (value) {
        if (Array.isArray(value)) {
            return null;
        }
        return new ListValidationError(value, this, 'Value must be an array');
    };
    ListToken.prototype.Encode = function (args) {
        var val = args.pop();
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        var schema = this.createToken(this.val.args[0], 0);
        return val.reduce(function (prev, current) {
            return __spread(prev, [schema.EncodeObject(current)]);
        }, []);
    };
    ListToken.prototype.Execute = function (val, semantics) {
        var schema = this.createToken(this.val.args[0], 0);
        var err = this.isValid(val);
        if (err) {
            throw err;
        }
        return val.reduce(function (prev, current) {
            return __spread(prev, [schema.Execute(current, semantics)]);
        }, []);
    };
    ListToken.prototype.EncodeObject = function (args) {
        var schema = this.createToken(this.val.args[0], 0);
        var err = this.isValid(args);
        if (err) {
            throw err;
        }
        return args.reduce(function (prev, current) {
            return __spread(prev, [schema.EncodeObject(current)]);
        }, []);
    };
    ListToken.prototype.ExtractSchema = function () {
        return ListToken.prim;
    };
    ListToken.prim = 'list';
    return ListToken;
}(token_1.Token));
exports.ListToken = ListToken;
//# sourceMappingURL=list.js.map