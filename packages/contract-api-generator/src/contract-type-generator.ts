import * as M from '@taquito/michel-codec';
import { GenerateApiError } from './generator/common';
import { visitContractStorage, visitContractParameter } from './generator/contract-parser';
import { SchemaOutput, toSchema } from './generator/schema-output';
import { toTypescriptCode } from './generator/typescript-output';

export const generateContractTypes = (contractScript: string): {
    schema: SchemaOutput;
    typescriptCode: {
        final: string;
        methods: string;
    };
} => {

    const p = new M.Parser();

    const contract = p.parseScript(contractScript) as M.MichelsonContract;
    if (!contract) {
        throw new GenerateApiError(`Could not parse contract script`, contractScript);
    }

    const contractStorage = contract.find(x => x.prim === `storage`) as undefined | M.MichelsonContractStorage;
    const contractParameter = contract.find(x => x.prim === `parameter`) as undefined | M.MichelsonContractParameter;

    const storageResult = contractStorage && visitContractStorage(contractStorage);
    const storage = storageResult ?? { storage: { raw: { prim: `never` } as M.MichelsonType, fields: [] } };

    const parameterResult = contractParameter && visitContractParameter(contractParameter);
    const methods = parameterResult?.methods ?? [];
    const schemaOutput = toSchema(methods);

    const typescriptCode = toTypescriptCode(storage, methods);

    return {
        schema: schemaOutput,
        typescriptCode,
    };
};
