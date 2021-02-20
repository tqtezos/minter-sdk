"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.tokens = void 0;
var pair_1 = require("./pair");
var nat_1 = require("./comparable/nat");
var string_1 = require("./comparable/string");
var bigmap_1 = require("./bigmap");
var address_1 = require("./comparable/address");
var map_1 = require("./map");
var bool_1 = require("./comparable/bool");
var or_1 = require("./or");
var contract_1 = require("./contract");
var list_1 = require("./list");
var mutez_1 = require("./comparable/mutez");
var bytes_1 = require("./comparable/bytes");
var option_1 = require("./option");
var timestamp_1 = require("./comparable/timestamp");
var int_1 = require("./comparable/int");
var unit_1 = require("./unit");
var key_1 = require("./key");
var key_hash_1 = require("./comparable/key_hash");
var signature_1 = require("./signature");
var lambda_1 = require("./lambda");
var operation_1 = require("./operation");
var set_1 = require("./set");
var chain_id_1 = require("./chain-id");
exports.tokens = [
    pair_1.PairToken,
    nat_1.NatToken,
    string_1.StringToken,
    bigmap_1.BigMapToken,
    address_1.AddressToken,
    map_1.MapToken,
    bool_1.BoolToken,
    or_1.OrToken,
    contract_1.ContractToken,
    list_1.ListToken,
    mutez_1.MutezToken,
    bytes_1.BytesToken,
    option_1.OptionToken,
    timestamp_1.TimestampToken,
    int_1.IntToken,
    unit_1.UnitToken,
    key_1.KeyToken,
    key_hash_1.KeyHashToken,
    signature_1.SignatureToken,
    lambda_1.LambdaToken,
    operation_1.OperationToken,
    set_1.SetToken,
    chain_id_1.ChainIDToken,
];
//# sourceMappingURL=tokens.js.map