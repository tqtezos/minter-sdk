import { Token, TokenFactory } from './token';
export declare class OperationToken extends Token {
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
    Execute(val: any): {
        [key: string]: any;
    };
    Encode(...args: any[]): any;
    EncodeObject(val: any): any;
    ExtractSchema(): string;
}
