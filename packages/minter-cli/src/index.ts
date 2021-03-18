import fsRaw from 'fs';
import path from 'path';
import { generateContractTypesFromMichelsonCode } from '@minter-sdk/contract-api-generator';
const fs = fsRaw.promises;

export const run = async (): Promise<void> => {
    const argv = process.argv;
    const argsGenerateFile = argv.some(a => a.startsWith(`--g`)) ? argv.slice(argv.findIndex(a => a.startsWith(`--g`)) + 1) : undefined;

    console.log(`minter-cli\n\t${argv.join(`\n\t`)}`);

    if (argsGenerateFile) {
        const [michelsonCodePath, outputTypescriptPath] = argsGenerateFile;
        console.log(`Generating Api: ${path.resolve(michelsonCodePath)} => ${path.resolve(outputTypescriptPath)}`);

        // Make dir
        await fs.mkdir(outputTypescriptPath, { recursive: true });

        const allFiles = await fs.readdir(michelsonCodePath);
        const files = allFiles.filter(x => x.endsWith(`.tz`));
        console.log(`Contracts Found: ${[``, ...files].join(`\n\t- `)}`);

        for (const fileRelativePath of files) {
            const inputFilePath = path.join(michelsonCodePath, fileRelativePath);
            const outputFilePath = path.join(outputTypescriptPath, fileRelativePath.replace(`.tz`, `.ts`));
            console.log(`Processing ${fileRelativePath}...`);

            try {
                const michelsonCode = await fs.readFile(inputFilePath, { encoding: `utf8` });
                const { typescriptCode } = generateContractTypesFromMichelsonCode(michelsonCode);
                await fs.writeFile(outputFilePath, typescriptCode);
            } catch (err: unknown) {
                console.error(`❌ Could not process ${fileRelativePath}`, { err });
            }
        }

        return;
    }

    console.log(`
minter-cli

Example usages:

minter-cli --g contract.tk contractTypes.ts
    `);
};

void run();
