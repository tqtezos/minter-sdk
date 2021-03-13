export const generateContractApiFromMichelsonCode = (michelsonCode: string): { typescriptCode: string } => {
    return { typescriptCode: `TS: ${michelsonCode}` };
};