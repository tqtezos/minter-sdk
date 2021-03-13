import { generateContractApi } from './contract-api-generator';

export const generateContractApiFromMichelsonCode = (michelsonCode: string): { typescriptCode: string } => {

    const result = generateContractApi(michelsonCode);

    return { typescriptCode: result.typescriptCode.final };
};