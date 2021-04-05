export type EnglishAuctionFa2CodeType = { __type: 'EnglishAuctionFa2CodeType'; code: object[]; };
export default { __type: 'EnglishAuctionFa2CodeType', code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"unit","annots":["%confirm_admin"]},{"prim":"bool","annots":["%pause"]}]},{"prim":"address","annots":["%set_admin"]}],"annots":["%admin"]},{"prim":"pair","args":[{"prim":"nat","annots":["%asset_id"]},{"prim":"nat","annots":["%bid_amount"]}],"annots":["%bid"]}]},{"prim":"or","args":[{"prim":"nat","annots":["%cancel"]},{"prim":"pair","args":[{"prim":"nat","annots":["%opening_price"]},{"prim":"pair","args":[{"prim":"nat","annots":["%min_raise_percent"]},{"prim":"pair","args":[{"prim":"nat","annots":["%min_raise"]},{"prim":"pair","args":[{"prim":"nat","annots":["%round_time"]},{"prim":"pair","args":[{"prim":"nat","annots":["%extend_time"]},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}],"annots":["%fa2_batch"]}]}],"annots":["%asset"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%start_time"]},{"prim":"timestamp","annots":["%end_time"]}]}]}]}]}]}]}],"annots":["%configure"]}]}]},{"prim":"nat","annots":["%resolve"]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%admin"]},{"prim":"bool","annots":["%paused"]}]},{"prim":"option","args":[{"prim":"address"}],"annots":["%pending_admin"]}]}],"annots":["%pauseable_admin"]},{"prim":"pair","args":[{"prim":"nat","annots":["%current_id"]},{"prim":"pair","args":[{"prim":"nat","annots":["%max_auction_time"]},{"prim":"pair","args":[{"prim":"nat","annots":["%max_config_to_start_time"]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"nat","annots":["%token_id"]}],"annots":["%bid_currency"]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"address","annots":["%seller"]},{"prim":"pair","args":[{"prim":"nat","annots":["%current_bid"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%start_time"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%last_bid_time"]},{"prim":"pair","args":[{"prim":"int","annots":["%round_time"]},{"prim":"pair","args":[{"prim":"int","annots":["%extend_time"]},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}],"annots":["%fa2_batch"]}]}],"annots":["%asset"]},{"prim":"pair","args":[{"prim":"nat","annots":["%min_raise_percent"]},{"prim":"pair","args":[{"prim":"nat","annots":["%min_raise"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%end_time"]},{"prim":"address","annots":["%highest_bidder"]}]}]}]}]}]}]}]}]}]}]}],"annots":["%auctions"]}]}]}]}]}]}]},{"prim":"code","args":[[{"prim":"LAMBDA","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]}]},{"prim":"unit"},[{"prim":"IF_NONE","args":[[{"prim":"UNIT"}],[{"prim":"CAR"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_AN_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]}]]},{"prim":"LAMBDA","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]}]},{"prim":"unit"},[{"prim":"IF_NONE","args":[[{"prim":"UNIT"}],[{"prim":"CAR"},{"prim":"CDR"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"PAUSED"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"bool"},{"prim":"string"}]},{"prim":"unit"},[{"prim":"UNPAIR"},{"prim":"NOT"},{"prim":"IF","args":[[{"prim":"FAILWITH"}],[{"prim":"DROP"},{"prim":"UNIT"}]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"operation"},[{"prim":"UNPAIR","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP"},{"prim":"CDR"},{"prim":"MAP","args":[[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"PAIR"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"CONTRACT","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%from_"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%to_"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}],"annots":["%txs"]}]}]}],"annots":["%transfer"]},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"Invalid FA2 Address"}]},{"prim":"FAILWITH"}],[]]},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"TRANSFER_TOKENS"}]]},{"prim":"DUP"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"operation"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"address"}]},{"prim":"address"}]}]},{"prim":"list","args":[{"prim":"operation"}]},[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"MAP","args":[[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DROP"}]]},{"prim":"SWAP"},{"prim":"APPLY"},{"prim":"SWAP"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"operation"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]}]}]},{"prim":"operation"},[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]},{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"EXEC"}]]},{"prim":"SWAP"},{"prim":"APPLY"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"address"}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]},{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"address"}]}]}]}]}]}]}]}]}]}]},[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"Auction does not exist for given asset_id"}]},{"prim":"FAILWITH"}],[]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"address"}]}]}]}]}]}]}]}]}]}]},{"prim":"bool"},[{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"NOW"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"NOW"},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"OR"}]]},{"prim":"DIG","args":[{"int":"7"}]},{"prim":"UNPAIR"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Amount must be 0mutez"}]},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"DUP"},{"prim":"CDR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_PENDING_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SOME"}],[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_A_PENDING_ADMIN"}]},{"prim":"FAILWITH"}]]}]]}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"DUP"},{"prim":"CDR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SOME"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SOME"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"7"}]},{"prim":"DROP"},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Bidder must be an implicit account"}]},{"prim":"SOURCE"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Auction must be in progress"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"NOT"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"NOW"},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"AND"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Seller cannot place a bid"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DUP"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"AND"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"100"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"MUL"},{"prim":"SWAP"},{"prim":"INT"},{"prim":"SWAP"},{"prim":"NEG"},{"prim":"EDIV"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"DIV by 0"}]},{"prim":"FAILWITH"}],[]]},{"prim":"CAR"},{"prim":"ABS"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"OR"},{"prim":"OR"},{"prim":"NOT"},{"prim":"IF","args":[[{"prim":"NOW"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Invalid Bid amount"}]},{"prim":"PAIR"},{"prim":"FAILWITH"}],[]]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"PAIR"},{"prim":"SELF_ADDRESS"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SELF_ADDRESS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"NOW"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SUB"},{"prim":"COMPARE"},{"prim":"LE"},{"prim":"IF","args":[[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"NOW"},{"prim":"ADD"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"}]]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SENDER"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"NOW"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"UPDATE"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CONS"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}],[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"8"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Only seller can cancel auction"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Auction must not have ended"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"NOT"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Amount must be 0mutez"}]},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CAR"},{"prim":"SELF_ADDRESS"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SELF_ADDRESS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"NONE","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"address"}]}]}]}]}]}]}]}]}]}]}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"7"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"end_time must be after start_time"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Auction time must be less than max_auction_time"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SUB"},{"prim":"ABS"},{"prim":"COMPARE"},{"prim":"LE"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Start_time must not have already passed"}]},{"prim":"NOW"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"GE"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"start_time must not be greater than the sum of current time and max_config_to_start_time"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"NOW"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SUB"},{"prim":"ABS"},{"prim":"COMPARE"},{"prim":"LE"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Opening price must be greater than 0 tokens"}]},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Amount must be 0mutez"}]},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Round_time must be greater than 0 seconds"}]},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SENDER"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"INT"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"INT"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"SELF_ADDRESS"},{"prim":"SENDER"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SELF_ADDRESS"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"UPDATE"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}]]}],[{"prim":"DIG","args":[{"int":"8"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Auction must have ended"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"Amount must be 0mutez"}]},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SELF_ADDRESS"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SELF_ADDRESS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"NONE","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"int"},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"timestamp"},{"prim":"address"}]}]}]}]}]}]}]}]}]}]}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}]]}]`) } as EnglishAuctionFa2CodeType;
