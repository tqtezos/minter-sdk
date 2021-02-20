"use strict";
var _a, _b;
Object.defineProperty(exports, "__esModule", { value: true });
exports.prefixLength = exports.prefix = exports.Prefix = void 0;
var Prefix;
(function (Prefix) {
    Prefix["TZ1"] = "tz1";
    Prefix["TZ2"] = "tz2";
    Prefix["TZ3"] = "tz3";
    Prefix["KT"] = "KT";
    Prefix["KT1"] = "KT1";
    Prefix["EDSK2"] = "edsk2";
    Prefix["SPSK"] = "spsk";
    Prefix["P2SK"] = "p2sk";
    Prefix["EDPK"] = "edpk";
    Prefix["SPPK"] = "sppk";
    Prefix["P2PK"] = "p2pk";
    Prefix["EDESK"] = "edesk";
    Prefix["SPESK"] = "spesk";
    Prefix["P2ESK"] = "p2esk";
    Prefix["EDSK"] = "edsk";
    Prefix["EDSIG"] = "edsig";
    Prefix["SPSIG"] = "spsig";
    Prefix["P2SIG"] = "p2sig";
    Prefix["SIG"] = "sig";
    Prefix["NET"] = "Net";
    Prefix["NCE"] = "nce";
    Prefix["B"] = "b";
    Prefix["O"] = "o";
    Prefix["LO"] = "Lo";
    Prefix["LLO"] = "LLo";
    Prefix["P"] = "P";
    Prefix["CO"] = "Co";
    Prefix["ID"] = "id";
    Prefix["EXPR"] = "expr";
    Prefix["TZ"] = "TZ";
})(Prefix = exports.Prefix || (exports.Prefix = {}));
exports.prefix = (_a = {},
    _a[Prefix.TZ1] = new Uint8Array([6, 161, 159]),
    _a[Prefix.TZ2] = new Uint8Array([6, 161, 161]),
    _a[Prefix.TZ3] = new Uint8Array([6, 161, 164]),
    _a[Prefix.KT] = new Uint8Array([2, 90, 121]),
    _a[Prefix.KT1] = new Uint8Array([2, 90, 121]),
    _a[Prefix.EDSK] = new Uint8Array([43, 246, 78, 7]),
    _a[Prefix.EDSK2] = new Uint8Array([13, 15, 58, 7]),
    _a[Prefix.SPSK] = new Uint8Array([17, 162, 224, 201]),
    _a[Prefix.P2SK] = new Uint8Array([16, 81, 238, 189]),
    _a[Prefix.EDPK] = new Uint8Array([13, 15, 37, 217]),
    _a[Prefix.SPPK] = new Uint8Array([3, 254, 226, 86]),
    _a[Prefix.P2PK] = new Uint8Array([3, 178, 139, 127]),
    _a[Prefix.EDESK] = new Uint8Array([7, 90, 60, 179, 41]),
    _a[Prefix.SPESK] = new Uint8Array([0x09, 0xed, 0xf1, 0xae, 0x96]),
    _a[Prefix.P2ESK] = new Uint8Array([0x09, 0x30, 0x39, 0x73, 0xab]),
    _a[Prefix.EDSIG] = new Uint8Array([9, 245, 205, 134, 18]),
    _a[Prefix.SPSIG] = new Uint8Array([13, 115, 101, 19, 63]),
    _a[Prefix.P2SIG] = new Uint8Array([54, 240, 44, 52]),
    _a[Prefix.SIG] = new Uint8Array([4, 130, 43]),
    _a[Prefix.NET] = new Uint8Array([87, 82, 0]),
    _a[Prefix.NCE] = new Uint8Array([69, 220, 169]),
    _a[Prefix.B] = new Uint8Array([1, 52]),
    _a[Prefix.O] = new Uint8Array([5, 116]),
    _a[Prefix.LO] = new Uint8Array([133, 233]),
    _a[Prefix.LLO] = new Uint8Array([29, 159, 109]),
    _a[Prefix.P] = new Uint8Array([2, 170]),
    _a[Prefix.CO] = new Uint8Array([79, 179]),
    _a[Prefix.ID] = new Uint8Array([153, 103]),
    _a[Prefix.EXPR] = new Uint8Array([13, 44, 64, 27]),
    // Legacy prefix
    _a[Prefix.TZ] = new Uint8Array([2, 90, 121]),
    _a);
exports.prefixLength = (_b = {},
    _b[Prefix.TZ1] = 20,
    _b[Prefix.TZ2] = 20,
    _b[Prefix.TZ3] = 20,
    _b[Prefix.KT] = 20,
    _b[Prefix.KT1] = 20,
    _b[Prefix.EDPK] = 32,
    _b[Prefix.SPPK] = 33,
    _b[Prefix.P2PK] = 33,
    _b[Prefix.EDSIG] = 64,
    _b[Prefix.SPSIG] = 64,
    _b[Prefix.P2SIG] = 64,
    _b[Prefix.SIG] = 64,
    _b[Prefix.NET] = 4,
    _b[Prefix.B] = 32,
    _b[Prefix.P] = 32,
    _b);
//# sourceMappingURL=constants.js.map