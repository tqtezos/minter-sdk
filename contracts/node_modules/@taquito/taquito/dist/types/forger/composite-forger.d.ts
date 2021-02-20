import { Forger, ForgeParams, ForgeResponse } from './interface';
export declare class ForgingMismatchError implements Error {
    results: string[];
    name: string;
    message: string;
    constructor(results: string[]);
}
export declare class CompositeForger implements Forger {
    private forgers;
    constructor(forgers: Forger[]);
    forge({ branch, contents }: ForgeParams): Promise<ForgeResponse>;
}
