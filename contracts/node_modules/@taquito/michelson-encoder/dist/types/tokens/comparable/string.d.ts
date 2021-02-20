import { TokenFactory, ComparableToken } from '../token';
export declare class StringToken extends ComparableToken {
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
