"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.createToken = exports.InvalidTokenError = void 0;
var tokens_1 = require("./tokens");
var InvalidTokenError = /** @class */ (function () {
    function InvalidTokenError(message, data) {
        this.message = message;
        this.data = data;
        this.name = 'Invalid token error';
    }
    return InvalidTokenError;
}());
exports.InvalidTokenError = InvalidTokenError;
function createToken(val, idx) {
    var t = tokens_1.tokens.find(function (x) { return x.prim === val.prim; });
    if (!t) {
        throw new InvalidTokenError('Malformed data expected a value with a valid prim property', val);
    }
    return new t(val, idx, createToken);
}
exports.createToken = createToken;
//# sourceMappingURL=createToken.js.map