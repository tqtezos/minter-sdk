import { Token, TokenFactory, Semantic } from './token';
export declare class OrToken extends Token {
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
    EncodeObject(args: any): any;
    Execute(val: any, semantics?: Semantic): any;
    private traversal;
    ExtractSchema(): any;
}
