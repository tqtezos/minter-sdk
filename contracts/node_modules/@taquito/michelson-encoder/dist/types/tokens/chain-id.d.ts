import { TokenFactory, ComparableToken, TokenValidationError } from './token';
export declare class ChainIDValidationError extends TokenValidationError {
    value: any;
    token: ChainIDToken;
    name: string;
    constructor(value: any, token: ChainIDToken, message: string);
}
export declare class ChainIDToken extends ComparableToken {
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
    Execute(val: any): string;
    ExtractSchema(): string;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    ToKey({ string }: any): any;
    ToBigMapKey(val: string): {
        key: {
            string: string;
        };
        type: {
            prim: string;
        };
    };
}
