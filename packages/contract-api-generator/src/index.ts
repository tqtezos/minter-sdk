import { generateContractTypes } from './contract-type-generator';

export const generateContractTypesFromMichelsonCode = (michelsonCode: string): { typescriptCode: string } => {

    const result = generateContractTypes(michelsonCode);

    return { typescriptCode: result.typescriptCode.final };
};