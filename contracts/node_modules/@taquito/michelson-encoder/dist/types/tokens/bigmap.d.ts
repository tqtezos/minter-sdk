import { ComparableToken, Semantic, Token, TokenFactory, TokenValidationError } from './token';
export declare class BigMapValidationError extends TokenValidationError {
    value: any;
    token: BigMapToken;
    name: string;
    constructor(value: any, token: BigMapToken, message: string);
}
export declare class BigMapToken extends Token {
    protected val: {
        prim: string;
        args: any[];
        annots?: any[];
    };
    protected idx: number;
    protected fac: TokenFactory;
    static prim: string;
    constructor(val: {
        prim: string;
        args: any[];
        annots?: any[];
    }, idx: number, fac: TokenFactory);
    get ValueSchema(): Token;
    get KeySchema(): ComparableToken;
    ExtractSchema(): {
        [x: number]: any;
    };
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(args: any): any;
    Execute(val: any[] | {
        int: string;
    }, semantic?: Semantic): any;
}
