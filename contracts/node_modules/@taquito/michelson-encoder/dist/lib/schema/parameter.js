"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ParameterSchema = void 0;
var createToken_1 = require("../tokens/createToken");
var token_1 = require("../tokens/token");
var or_1 = require("../tokens/or");
var option_1 = require("../tokens/option");
/**
 * @warn Our current smart contract abstraction feature is currently in preview. It's API is not final, and it may not cover every use case (yet). We will greatly appreciate any feedback on this feature.
 */
var ParameterSchema = /** @class */ (function () {
    function ParameterSchema(val) {
        this.root = createToken_1.createToken(val, 0);
    }
    ParameterSchema.fromRPCResponse = function (val) {
        var parameter = val &&
            val.script &&
            Array.isArray(val.script.code) &&
            val.script.code.find(function (x) { return x.prim === 'parameter'; });
        if (!parameter || !Array.isArray(parameter.args)) {
            throw new Error('Invalid rpc response passed as arguments');
        }
        return new ParameterSchema(parameter.args[0]);
    };
    Object.defineProperty(ParameterSchema.prototype, "isMultipleEntryPoint", {
        get: function () {
            return (this.root instanceof or_1.OrToken ||
                (this.root instanceof option_1.OptionToken && this.root.subToken() instanceof or_1.OrToken));
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ParameterSchema.prototype, "hasAnnotation", {
        get: function () {
            if (this.isMultipleEntryPoint) {
                return Object.keys(this.ExtractSchema())[0] !== '0';
            }
            else {
                return true;
            }
        },
        enumerable: false,
        configurable: true
    });
    ParameterSchema.prototype.Execute = function (val, semantics) {
        return this.root.Execute(val, semantics);
    };
    ParameterSchema.prototype.Encode = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        try {
            return this.root.Encode(args.reverse());
        }
        catch (ex) {
            if (ex instanceof token_1.TokenValidationError) {
                throw ex;
            }
            throw new Error("Unable to encode storage object. " + ex);
        }
    };
    ParameterSchema.prototype.ExtractSchema = function () {
        return this.root.ExtractSchema();
    };
    ParameterSchema.prototype.ExtractSignatures = function () {
        return this.root.ExtractSignature();
    };
    return ParameterSchema;
}());
exports.ParameterSchema = ParameterSchema;
//# sourceMappingURL=parameter.js.map