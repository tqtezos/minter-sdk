import { Token, TokenFactory, TokenValidationError } from './token';
export declare class SignatureValidationError extends TokenValidationError {
    value: any;
    token: SignatureToken;
    name: string;
    constructor(value: any, token: SignatureToken, message: string);
}
export declare class SignatureToken extends Token {
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
    Execute(val: any): {
        [key: string]: any;
    };
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    ExtractSchema(): string;
}
