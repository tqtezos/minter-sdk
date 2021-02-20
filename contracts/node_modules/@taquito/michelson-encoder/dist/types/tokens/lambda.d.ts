import { Token, TokenFactory } from './token';
export declare class LambdaToken extends Token {
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
    Encode(args: any[]): any;
    EncodeObject(val: any): any;
    ExtractSchema(): {
        [x: string]: {
            parameters: any;
            returns: any;
        };
    };
}
