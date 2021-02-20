import { TokenFactory, ComparableToken, TokenValidationError } from '../token';
export declare class IntValidationError extends TokenValidationError {
    value: any;
    token: IntToken;
    name: string;
    constructor(value: any, token: IntToken, message: string);
}
export declare class IntToken extends ComparableToken {
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
        [key: string]: string;
    }): {
        [key: string]: any;
    };
    ExtractSchema(): string;
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
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
