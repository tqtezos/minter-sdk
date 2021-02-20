"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.RpcInjector = void 0;
var RpcInjector = /** @class */ (function () {
    function RpcInjector(context) {
        this.context = context;
    }
    RpcInjector.prototype.inject = function (signedOperationBytes) {
        return this.context.rpc.injectOperation(signedOperationBytes);
    };
    return RpcInjector;
}());
exports.RpcInjector = RpcInjector;
//# sourceMappingURL=rpc-injector.js.map