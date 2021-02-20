import { TokenFactory, ComparableToken, TokenValidationError } from '../token';
import BigNumber from 'bignumber.js';
export declare class MutezValidationError extends TokenValidationError {
    value: any;
    token: MutezToken;
    name: string;
    constructor(value: any, token: MutezToken, message: string);
}
export declare class MutezToken extends ComparableToken {
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
    Execute(val: any): BigNumber;
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
