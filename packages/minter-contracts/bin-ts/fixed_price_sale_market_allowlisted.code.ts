
export const FixedPriceSaleMarketAllowlistedCode: { __type: 'FixedPriceSaleMarketAllowlistedCode', protocol: string, code: object[] } = {
    __type: 'FixedPriceSaleMarketAllowlistedCode',
    protocol: 'PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo',
    code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","annots":["%call_market"],"args":[{"prim":"or","args":[{"prim":"or","annots":["%admin"],"args":[{"prim":"or","args":[{"prim":"unit","annots":["%confirm_admin"]},{"prim":"bool","annots":["%pause"]}]},{"prim":"address","annots":["%set_admin"]}]},{"prim":"pair","annots":["%buy"],"args":[{"prim":"address","annots":["%seller"]},{"prim":"pair","annots":["%tokens"],"args":[{"prim":"address","annots":["%token_for_sale_address"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_for_sale_token_id"]},{"prim":"pair","args":[{"prim":"address","annots":["%money_token_address"]},{"prim":"nat","annots":["%money_token_token_id"]}]}]}]}]}]},{"prim":"or","args":[{"prim":"pair","annots":["%cancel"],"args":[{"prim":"address","annots":["%seller"]},{"prim":"pair","annots":["%tokens"],"args":[{"prim":"address","annots":["%token_for_sale_address"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_for_sale_token_id"]},{"prim":"pair","args":[{"prim":"address","annots":["%money_token_address"]},{"prim":"nat","annots":["%money_token_token_id"]}]}]}]}]},{"prim":"pair","annots":["%sell"],"args":[{"prim":"nat","annots":["%sale_price"]},{"prim":"pair","annots":["%sale_tokens_param"],"args":[{"prim":"address","annots":["%token_for_sale_address"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_for_sale_token_id"]},{"prim":"pair","args":[{"prim":"address","annots":["%money_token_address"]},{"prim":"nat","annots":["%money_token_token_id"]}]}]}]}]}]}]},{"prim":"big_map","annots":["%update_allowed"],"args":[{"prim":"address"},{"prim":"unit"}]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"big_map","annots":["%allowlist"],"args":[{"prim":"address"},{"prim":"unit"}]},{"prim":"pair","annots":["%market_storage"],"args":[{"prim":"pair","annots":["%admin"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%admin"]},{"prim":"bool","annots":["%paused"]}]},{"prim":"option","annots":["%pending_admin"],"args":[{"prim":"address"}]}]},{"prim":"big_map","annots":["%sales"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%seller"]},{"prim":"pair","annots":["%tokens"],"args":[{"prim":"address","annots":["%token_for_sale_address"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_for_sale_token_id"]},{"prim":"pair","args":[{"prim":"address","annots":["%money_token_address"]},{"prim":"nat","annots":["%money_token_token_id"]}]}]}]}]},{"prim":"nat"}]}]}]}]},{"prim":"code","args":[[{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]},{"prim":"unit"},[{"prim":"CAR"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_AN_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"address"}]}]},{"prim":"address"}]},{"prim":"operation"},[[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"DIG","args":[{"int":"2"}]},[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONTRACT","annots":["%transfer"],"args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%from_"]},{"prim":"list","annots":["%txs"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%to_"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]}]}]},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"4"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"CANNOT_INVOKE_MONEY_FA2"}]},{"prim":"FAILWITH"}],[{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"TRANSFER_TOKENS"}]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"big_map","args":[{"prim":"address"},{"prim":"unit"}]}]},{"prim":"string"}]},{"prim":"unit"},[[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"DROP"}]]}]]},{"prim":"DIG","args":[{"int":"3"}]},[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"IF_LEFT","args":[[{"prim":"DROP"},{"prim":"UNIT"}],[{"prim":"DROP"},{"prim":"UNIT"}]]}],[{"prim":"IF_LEFT","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"UNIT"}],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SALE_ADDRESS_NOT_ALLOWED"}]},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"5"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"6"}]}],{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"MONEY_ADDRESS_NOT_ALLOWED"}]},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"UNIT"}]]}]]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_PENDING_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"}],[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_A_PENDING_ADMIN"}]},{"prim":"FAILWITH"}]]}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"PAUSED"}]},{"prim":"FAILWITH"}],[]]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_SALE"}]},{"prim":"FAILWITH"}],[]]},{"prim":"SENDER"},{"prim":"SELF_ADDRESS"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},[{"prim":"DIP","args":[{"int":"4"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"5"}]}],{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"5"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"6"}]}],{"prim":"SWAP"},{"prim":"EXEC"},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CAR"},{"prim":"SENDER"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},[{"prim":"DIP","args":[{"int":"4"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"5"}]}],{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"NONE","args":[{"prim":"nat"}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CONS"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}],[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"unit"},{"prim":"Unit"}]}],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"OR A SELLER"}]},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CAR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":" "}]},{"prim":"CONCAT"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_AN_ADMIN"}]},{"prim":"CONCAT"},{"prim":"FAILWITH"}],[{"prim":"DROP"},{"prim":"UNIT"}]]}]]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_SALE"}]},{"prim":"FAILWITH"}],[{"prim":"DROP"},{"prim":"SENDER"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"SENDER"},{"prim":"SELF_ADDRESS"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"NONE","args":[{"prim":"nat"}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}],[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_OWNER"}]},{"prim":"FAILWITH"}]]}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SELF_ADDRESS"},{"prim":"SENDER"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"2"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"3"}]}],{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},[{"prim":"DIP","args":[{"int":"3"},[{"prim":"DUP"}]]},{"prim":"DIG","args":[{"int":"4"}]}],{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}]]},[[{"prim":"DUP"},{"prim":"CAR"},{"prim":"DIP","args":[[{"prim":"CDR"}]]}]],{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}]]}]`)
};
