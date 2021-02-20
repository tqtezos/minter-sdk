import { TokenFactory, ComparableToken, TokenValidationError } from '../token';
export declare class BytesValidationError extends TokenValidationError {
    value: any;
    token: BytesToken;
    name: string;
    constructor(value: any, token: BytesToken, message: string);
}
export declare class BytesToken extends ComparableToken {
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
    ToBigMapKey(val: string): {
        key: {
            bytes: string;
        };
        type: {
            prim: string;
        };
    };
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(val: any): {
        bytes: string;
    };
    Execute(val: any): string;
    ExtractSchema(): string;
    ToKey({ bytes, string }: any): any;
}
