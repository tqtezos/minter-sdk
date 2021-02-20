"use strict";
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
exports.ObservableSubscription = void 0;
var rxjs_1 = require("rxjs");
var operators_1 = require("rxjs/operators");
var ObservableSubscription = /** @class */ (function () {
    function ObservableSubscription(obs, shouldRetry) {
        var _this = this;
        if (shouldRetry === void 0) { shouldRetry = false; }
        this.shouldRetry = shouldRetry;
        this.errorListeners = [];
        this.messageListeners = [];
        this.closeListeners = [];
        this.completed$ = new rxjs_1.Subject();
        obs
            .pipe(operators_1.takeUntil(this.completed$), operators_1.tap(function (data) {
            _this.call(_this.messageListeners, data);
        }, function (error) {
            _this.call(_this.errorListeners, error);
        }, function () {
            _this.call(_this.closeListeners);
        }), this.shouldRetry ? operators_1.retry() : operators_1.tap(), operators_1.catchError(function () { return rxjs_1.NEVER; }))
            .subscribe();
    }
    ObservableSubscription.prototype.call = function (listeners, value) {
        var e_1, _a;
        try {
            for (var listeners_1 = __values(listeners), listeners_1_1 = listeners_1.next(); !listeners_1_1.done; listeners_1_1 = listeners_1.next()) {
                var l = listeners_1_1.value;
                try {
                    l(value);
                }
                catch (ex) {
                    console.error(ex);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (listeners_1_1 && !listeners_1_1.done && (_a = listeners_1.return)) _a.call(listeners_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
    };
    ObservableSubscription.prototype.remove = function (listeners, value) {
        var idx = listeners.indexOf(value);
        if (idx !== -1) {
            listeners.splice(idx, 1);
        }
    };
    ObservableSubscription.prototype.on = function (type, cb) {
        switch (type) {
            case 'data':
                this.messageListeners.push(cb);
                break;
            case 'error':
                this.errorListeners.push(cb);
                break;
            case 'close':
                this.closeListeners.push(cb);
                break;
            default:
                throw new Error("Trying to register on an unsupported event: " + type);
        }
    };
    ObservableSubscription.prototype.off = function (type, cb) {
        switch (type) {
            case 'data':
                this.remove(this.messageListeners, cb);
                break;
            case 'error':
                this.remove(this.errorListeners, cb);
                break;
            case 'close':
                this.remove(this.closeListeners, cb);
                break;
            default:
                throw new Error("Trying to unregister on an unsupported event: " + type);
        }
    };
    ObservableSubscription.prototype.close = function () {
        this.completed$.next();
    };
    return ObservableSubscription;
}());
exports.ObservableSubscription = ObservableSubscription;
//# sourceMappingURL=observable-subscription.js.map