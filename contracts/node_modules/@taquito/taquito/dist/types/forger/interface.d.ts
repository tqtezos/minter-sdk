import { OperationContents } from '@taquito/rpc';
export interface ForgeParams {
    branch: string;
    contents: OperationContents[];
}
export declare type ForgeResponse = string;
export interface Forger {
    forge(params: ForgeParams): Promise<ForgeResponse>;
}
