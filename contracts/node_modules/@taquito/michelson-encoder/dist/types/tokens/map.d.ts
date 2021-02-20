import { ComparableToken, Semantic, Token, TokenFactory, TokenValidationError } from './token';
export declare class MapValidationError extends TokenValidationError {
    value: any;
    token: MapToken;
    name: string;
    constructor(value: any, token: MapToken, message: string);
}
export declare class MapToken extends Token {
    protected val: {
        prim: string;
        args: any[];
        annots: any[];
    };
    protected idx: number;
    protected fac: TokenFactory;
    static prim: string;
    constructor(val: {
        prim: string;
        args: any[];
        annots: any[];
    }, idx: number, fac: TokenFactory);
    get ValueSchema(): Token;
    get KeySchema(): ComparableToken;
    private isValid;
    Execute(val: any[], semantics?: Semantic): {
        [key: string]: any;
    };
    Encode(args: any[]): any;
    EncodeObject(args: any): any;
    ExtractSchema(): {
        map: {
            key: any;
            value: any;
        };
    };
}
