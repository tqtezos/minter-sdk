import { TokenFactory, Semantic, ComparableToken } from './token';
export declare class PairToken extends ComparableToken {
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
    Encode(args: any[]): any;
    ExtractSignature(): any;
    ToBigMapKey(val: any): {
        key: any;
        type: {
            prim: string;
            args?: any[] | undefined;
        };
    };
    ToKey(val: any): {
        [key: string]: any;
    };
    EncodeObject(args: any): any;
    private traversal;
    Execute(val: any, semantics?: Semantic): {
        [key: string]: any;
    };
    ExtractSchema(): any;
    compare(val1: any, val2: any): number;
}
