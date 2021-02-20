import { TokenFactory, ComparableToken, TokenValidationError } from '../token';
export declare class NatValidationError extends TokenValidationError {
    value: any;
    token: NatToken;
    name: string;
    constructor(value: any, token: NatToken, message: string);
}
export declare class NatToken extends ComparableToken {
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
    Encode(args: any[]): any;
    private isValid;
    EncodeObject(val: any): any;
    ExtractSchema(): string;
    ToBigMapKey(val: string): {
        key: {
            int: string;
        };
        type: {
            prim: string;
        };
    };
    ToKey({ int }: any): any;
}
