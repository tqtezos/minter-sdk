import { TokenFactory, ComparableToken, TokenValidationError } from '../token';
export declare class AddressValidationError extends TokenValidationError {
    value: any;
    token: AddressToken;
    name: string;
    constructor(value: any, token: AddressToken, message: string);
}
export declare class AddressToken extends ComparableToken {
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
    ToBigMapKey(val: any): {
        key: {
            bytes: string;
        };
        type: {
            prim: string;
        };
    };
    private isValid;
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    Execute(val: {
        bytes: string;
        string: string;
    }): string;
    ExtractSchema(): string;
    ToKey({ bytes, string }: any): any;
    compare(address1: string, address2: string): number;
}
