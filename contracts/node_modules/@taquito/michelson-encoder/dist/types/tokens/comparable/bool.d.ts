import { TokenFactory, ComparableToken } from '../token';
export declare class BoolToken extends ComparableToken {
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
    Execute(val: any): boolean;
    Encode(args: any[]): any;
    EncodeObject(val: any): {
        prim: string;
    };
    ExtractSchema(): string;
    ToBigMapKey(val: string): {
        key: {
            [key: string]: string;
        };
        type: {
            prim: string;
        };
    };
    ToKey(val: string): {
        prim: string;
    };
    compare(val1: any, val2: any): 0 | 1 | -1;
}
