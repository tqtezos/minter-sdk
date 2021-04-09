export type Fa2MultiNftTokenEditionsCodeType = { __type: 'Fa2MultiNftTokenEditionsCodeType'; code: object[]; };
export default { __type: 'Fa2MultiNftTokenEditionsCodeType', code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%edition_id"]},{"prim":"list","args":[{"prim":"address"}],"annots":["%receivers"]}]}],"annots":["%distribute_editions"]},{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"unit","annots":["%confirm_admin"]},{"prim":"bool","annots":["%pause"]}]},{"prim":"address","annots":["%set_admin"]}],"annots":["%admin"]},{"prim":"or","args":[{"prim":"or","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%requests"]},{"prim":"contract","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"nat","annots":["%token_id"]}],"annots":["%request"]},{"prim":"nat","annots":["%balance"]}]}]}],"annots":["%callback"]}],"annots":["%balance_of"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%from_"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%to_"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}],"annots":["%txs"]}]}],"annots":["%transfer"]}]},{"prim":"list","args":[{"prim":"or","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"address","annots":["%operator"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%add_operator"]},{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"address","annots":["%operator"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%remove_operator"]}]}],"annots":["%update_operators"]}],"annots":["%assets"]}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%token_info"]}],"annots":["%token_metadata"]},{"prim":"address","annots":["%owner"]}]}],"annots":["%mint"]}],"annots":["%fA2"]}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%edition_info"]},{"prim":"nat","annots":["%number_of_editions"]}]}],"annots":["%mint_editions"]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%current_edition_id"]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"address","annots":["%creator"]},{"prim":"pair","args":[{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%edition_info"]},{"prim":"pair","args":[{"prim":"nat","annots":["%initial_token_id"]},{"prim":"pair","args":[{"prim":"nat","annots":["%number_of_editions"]},{"prim":"int","annots":["%number_of_editions_to_distribute"]}]}]}]}]}],"annots":["%editions_metadata"]}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%admin"]},{"prim":"bool","annots":["%paused"]}]},{"prim":"option","args":[{"prim":"address"}],"annots":["%pending_admin"]}],"annots":["%admin"]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"nat"},{"prim":"address"}],"annots":["%ledger"]},{"prim":"nat","annots":["%next_token_id"]}]},{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}],"annots":["%operators"]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%token_info"]}]}],"annots":["%token_metadata"]}]}],"annots":["%assets"]}]},{"prim":"big_map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%metadata"]}],"annots":["%nft_asset_storage"]}]}]},{"prim":"code","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_TOKEN_UNDEFINED"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_INSUFFICIENT_BALANCE"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"string"},{"prim":"string"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]}]}]},{"prim":"unit"}]}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"nat"},{"prim":"address"}]},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}]}]}]}]}]}]}]},{"prim":"pair","args":[{"prim":"list","args":[{"prim":"operation"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"nat"},{"prim":"address"}]},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}]}]}]}]}]}]},[{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"IF_NONE","args":[[{"prim":"UNIT"}],[{"prim":"DUP","args":[{"int":"5"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SENDER"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"FAILWITH"}],[{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"DUP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"FAILWITH"}],[{"prim":"DROP"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"FAILWITH"}],[{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"SWAP"},{"prim":"UPDATE"}],[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"FAILWITH"}]]}]]}]]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"}]]}]]}]]}]]},{"prim":"SWAP"},{"prim":"DROP"}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DROP","args":[{"int":"2"}]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"SWAP"},{"prim":"APPLY"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]},{"prim":"unit"},[{"prim":"CAR"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_AN_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]},{"prim":"unit"},[{"prim":"CAR"},{"prim":"CDR"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"PAUSED"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"UNPAIR"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"INVALID_EDITION_ID"}]},{"prim":"FAILWITH"}],[]]},{"prim":"DUP"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"INVALID_DISTRIBUTOR"}]},{"prim":"FAILWITH"}],[]]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"EMPTY_MAP","args":[{"prim":"string"},{"prim":"bytes"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"PACK"},{"prim":"SOME"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"edition_id"}]},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}]}]},{"prim":"address"}]}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"UNPAIR"},{"prim":"PUSH","args":[{"prim":"int"},{"int":"0"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"GT"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_EDITIONS_TO_DISTRIBUTE"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SUB"},{"prim":"ABS"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"PUSH","args":[{"prim":"int"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SUB"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"MEM"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"3"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_INVALID_TOKEN_ID"}]},{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"SOME"},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"PAIR"}]]}]]},{"prim":"DUP"},{"prim":"CDR"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]}]}]},{"prim":"unit"},[{"prim":"DROP"},{"prim":"UNIT"}]]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_PENDING_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"}],[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_A_PENDING_ADMIN"}]},{"prim":"FAILWITH"}]]}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"CAR"},{"prim":"MAP","args":[[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]}],[{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]}]]},{"prim":"SWAP"},{"prim":"PAIR"}]]}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"TRANSFER_TOKENS"},{"prim":"SWAP"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"MAP","args":[[{"prim":"DUP"},{"prim":"CDR"},{"prim":"MAP","args":[[{"prim":"DUP"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"SOME"},{"prim":"PAIR"}]]},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"SOME"},{"prim":"PAIR"}]]},{"prim":"SWAP"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]}]}]},{"prim":"unit"},[{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"4"}]},{"prim":"UNIT"}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"MEM"},{"prim":"IF","args":[[{"prim":"UNIT"}],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_NOT_OPERATOR"}]},{"prim":"FAILWITH"}]]}]]}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]}],[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"SENDER"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"IF_LEFT","args":[[],[]]},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_NOT_OWNER"}]},{"prim":"FAILWITH"}]]},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"UNIT"},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"UPDATE"}],[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NONE","args":[{"prim":"unit"}]},{"prim":"SWAP"},{"prim":"UPDATE"}]]}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"MEM"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"3"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_INVALID_TOKEN_ID"}]},{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"ADD"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"SOME"},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"PAIR"}]]}]]},{"prim":"DUP"},{"prim":"CDR"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]}]}]},{"prim":"unit"},[{"prim":"DROP"},{"prim":"UNIT"}]]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"PAIR"},{"prim":"CONS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]}],[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"INT"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"ADD"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DUP"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}]]}]`) } as Fa2MultiNftTokenEditionsCodeType;
