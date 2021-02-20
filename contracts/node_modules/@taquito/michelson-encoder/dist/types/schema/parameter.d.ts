import { Semantic } from '../tokens/token';
import { ScriptResponse, MichelsonV1Expression } from '@taquito/rpc';
/**
 * @warn Our current smart contract abstraction feature is currently in preview. It's API is not final, and it may not cover every use case (yet). We will greatly appreciate any feedback on this feature.
 */
export declare class ParameterSchema {
    private root;
    static fromRPCResponse(val: {
        script: ScriptResponse;
    }): ParameterSchema;
    get isMultipleEntryPoint(): boolean;
    get hasAnnotation(): boolean;
    constructor(val: MichelsonV1Expression);
    Execute(val: any, semantics?: Semantic): any;
    Encode(...args: any[]): any;
    ExtractSchema(): any;
    ExtractSignatures(): any[][];
}
