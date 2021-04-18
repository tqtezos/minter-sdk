
export const TicketWalletCode: { __type: 'TicketWalletCode', protocol: string, code: object[] } = {
    __type: 'TicketWalletCode',
    protocol: 'PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA',
    code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"pair","annots":["%auction"],"args":[{"prim":"contract","annots":["%destination"],"args":[{"prim":"pair","args":[{"prim":"nat","annots":["%opening_price"]},{"prim":"pair","args":[{"prim":"nat","annots":["%set_reserve_price"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%set_start_time"]},{"prim":"pair","args":[{"prim":"int","annots":["%set_round_time"]},{"prim":"ticket","annots":["%ticket"],"args":[{"prim":"nat"}]}]}]}]}]}]},{"prim":"pair","args":[{"prim":"nat","annots":["%opening_price"]},{"prim":"pair","args":[{"prim":"nat","annots":["%reserve_price"]},{"prim":"pair","args":[{"prim":"timestamp","annots":["%start_time"]},{"prim":"pair","args":[{"prim":"int","annots":["%round_time"]},{"prim":"nat","annots":["%ticket_id"]}]}]}]}]}]},{"prim":"nat","annots":["%burn"]}]},{"prim":"or","args":[{"prim":"map","annots":["%mint"],"args":[{"prim":"string"},{"prim":"bytes"}]},{"prim":"ticket","annots":["%receive"],"args":[{"prim":"nat"}]}]}]},{"prim":"pair","annots":["%send"],"args":[{"prim":"contract","annots":["%destination"],"args":[{"prim":"ticket","args":[{"prim":"nat"}]}]},{"prim":"nat","annots":["%ticket_id"]}]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"address","annots":["%admin"]},{"prim":"pair","args":[{"prim":"big_map","annots":["%tickets"],"args":[{"prim":"nat"},{"prim":"ticket","args":[{"prim":"nat"}]}]},{"prim":"pair","args":[{"prim":"nat","annots":["%current_id"]},{"prim":"big_map","annots":["%token_metadata"],"args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}]}]}]}]}]}]}]},{"prim":"code","args":[[{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"UNPAIR","args":[{"int":"4"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"NONE","args":[{"prim":"ticket","args":[{"prim":"nat"}]}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"GET_AND_UPDATE"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"5"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"no tickets"}]},{"prim":"FAILWITH"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"TRANSFER_TOKENS"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"NONE","args":[{"prim":"ticket","args":[{"prim":"nat"}]}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"UPDATE"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"TICKET"},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"GET_AND_UPDATE"},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"PAIR"},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"UPDATE"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"READ_TICKET"},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"GET_AND_UPDATE"},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"failed assertion"}]},{"prim":"FAILWITH"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"NONE","args":[{"prim":"ticket","args":[{"prim":"nat"}]}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"GET_AND_UPDATE"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"5"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"no tickets"}]},{"prim":"FAILWITH"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"TRANSFER_TOKENS"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}]]}]]}]]}]`)
};
