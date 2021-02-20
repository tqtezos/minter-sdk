import { Token, TokenFactory, TokenValidationError } from './token';
export declare class KeyValidationError extends TokenValidationError {
    value: any;
    token: KeyToken;
    name: string;
    constructor(value: any, token: KeyToken, message: string);
}
export declare class KeyToken extends Token {
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
    Execute(val: {
        bytes: string;
        string: string;
    }): string;
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    ExtractSchema(): string;
}
