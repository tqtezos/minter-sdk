import { Token, TokenFactory } from './token';
export declare class UnitToken extends Token {
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
    EncodeObject(_val: any): any;
    Execute(): symbol;
    ExtractSchema(): string;
}
