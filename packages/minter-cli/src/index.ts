import fs from 'fs';
import { generateContractApiFromMichelsonCode } from '@minter-sdk/contract-api-generator';

export const run = async (): Promise<void> => {
    const argv = process.argv;
    const argsGenerateFile = (argv.includes(`--g`) || argv.includes(`--generate`)) ? argv.slice(argv.findIndex(a => a.startsWith(`--g`)) + 1, 2) : undefined;

    if (argsGenerateFile) {
        const [michelsonCodeFilePath, outputTypescriptFilePath] = argsGenerateFile;
        const michelsonCode = await fs.promises.readFile(michelsonCodeFilePath, { encoding: `utf8` });
        const { typescriptCode } = generateContractApiFromMichelsonCode(michelsonCode);
        await fs.promises.writeFile(outputTypescriptFilePath, typescriptCode);

        return;
    }

    console.log(`
minter-cli

Example usages:

minter-cli --g contract.tk contractTypes.ts
    `);
};

void run();
