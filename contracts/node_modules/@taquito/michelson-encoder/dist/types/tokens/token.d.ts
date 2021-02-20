import { MichelsonV1Expression } from '@taquito/rpc';
export declare abstract class TokenValidationError implements Error {
    value: any;
    token: Token;
    name: string;
    message: string;
    constructor(value: any, token: Token, baseMessage: string);
}
export declare type TokenFactory = (val: any, idx: number) => Token;
export interface Semantic {
    [key: string]: (value: MichelsonV1Expression, schema: MichelsonV1Expression) => any;
}
export declare abstract class Token {
    protected val: {
        prim: string;
        args: any[];
        annots?: any[];
    };
    protected idx: number;
    protected fac: TokenFactory;
    constructor(val: {
        prim: string;
        args: any[];
        annots?: any[];
    }, idx: number, fac: TokenFactory);
    protected typeWithoutAnnotations(): {
        prim: string;
        args?: any[] | undefined;
    };
    annot(): any;
    hasAnnotations(): number | false;
    createToken: TokenFactory;
    abstract ExtractSchema(): any;
    abstract Execute(val: any, semantics?: Semantic): any;
    abstract Encode(_args: any[]): any;
    abstract EncodeObject(args: any): any;
    ExtractSignature(): any[][];
}
export declare abstract class ComparableToken extends Token {
    abstract ToBigMapKey(val: string): {
        key: {
            [key: string]: string;
        };
        type: {
            prim: string;
        };
    };
    abstract ToKey(val: string): any;
    compare(o1: string, o2: string): number;
}
