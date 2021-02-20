import { Token, TokenFactory, Semantic, TokenValidationError } from './token';
export declare class ListValidationError extends TokenValidationError {
    value: any;
    token: ListToken;
    name: string;
    constructor(value: any, token: ListToken, message: string);
}
export declare class ListToken extends Token {
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
    private isValid;
    Encode(args: any[]): any;
    Execute(val: any, semantics?: Semantic): any;
    EncodeObject(args: any): any;
    ExtractSchema(): string;
}
