import { Token, TokenFactory, Semantic, TokenValidationError, ComparableToken } from './token';
export declare class SetValidationError extends TokenValidationError {
    value: any;
    token: SetToken;
    name: string;
    constructor(value: any, token: SetToken, message: string);
}
export declare class SetToken extends Token {
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
    get KeySchema(): ComparableToken;
    private isValid;
    Encode(args: any[]): any;
    Execute(val: any, semantics?: Semantic): any;
    EncodeObject(args: any): any;
    ExtractSchema(): string;
}
