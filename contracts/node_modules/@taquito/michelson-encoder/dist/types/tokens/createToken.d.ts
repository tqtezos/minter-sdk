import { Token } from './token';
export declare class InvalidTokenError implements Error {
    message: string;
    data: any;
    name: string;
    constructor(message: string, data: any);
}
export declare function createToken(val: any, idx: number): Token;
