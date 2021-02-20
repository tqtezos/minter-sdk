import { Token, TokenFactory, Semantic } from './token';
export declare class OptionToken extends Token {
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
    subToken(): Token;
    annot(): string;
    Encode(args: any): any;
    EncodeObject(args: any): any;
    Execute(val: any, semantics?: Semantic): any;
    ExtractSchema(): any;
    ExtractSignature(): any[][];
}
