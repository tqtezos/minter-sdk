import { Token, TokenFactory, TokenValidationError } from './token';
export declare class ContractValidationError extends TokenValidationError {
    value: any;
    token: ContractToken;
    name: string;
    constructor(value: any, token: ContractToken, message: string);
}
export declare class ContractToken extends Token {
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
    Execute(val: {
        bytes: string;
        string: string;
    }): any;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    ExtractSchema(): string;
}
