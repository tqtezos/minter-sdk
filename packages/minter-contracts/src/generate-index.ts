import path from 'path';
import fs from 'fs';
import { $log } from '@tsed/logger';
import yargs from 'yargs';

const args = yargs
  .option('path', {
    alias: 'p',
    describe: 'TypeScript files path',
    type: 'string',
    default: path.join(__dirname, '..', 'bin-ts'),
  })
  .help()
  .argv;

const typesPath = args.path;

const fileNameToTitleCase = (fileName: string) =>
  fileName
    .split('.')[0]
    .split('_')
    .map(x => x[0].toUpperCase() + x.substring(1))
    .join('');
const fileNameToContractType = (fileName: string) => fileNameToTitleCase(fileName) + 'ContractType';
const fileNameToCodeObject = (fileName: string) => fileNameToTitleCase(fileName) + 'Code';

const generateIndex = (codeFiles: string[]) => {
  const fileNames = codeFiles
    .map(x => x.replace('.code.ts', ''));

  const codeImports = fileNames
    .map(fileName =>
      `import { ${fileNameToCodeObject(fileName)} } from './${path.relative(process.cwd(), typesPath)}/${fileName}.code';\n` +
      `import { ${fileNameToContractType(fileName)} } from './${path.relative(process.cwd(), typesPath)}/${fileName}.types';`
      ,
    )
    .join('\n');

  const exports = codeFiles
    .map(fileName => `  ${fileNameToContractType(fileName)},\n  ${fileNameToCodeObject(fileName)},`)
    .join('\n');

  return `${codeImports}\n\nexport {\n${exports}\n};\n`;
};

try {
  const outputFiles = fs.readdirSync(typesPath);
  const codeFiles = outputFiles
    .filter(x => x.endsWith('.code.ts'));

  $log.info('Writing index');
  fs.writeFileSync('index.ts', generateIndex(codeFiles));
  process.exit(0);
} catch (e) {
  $log.error(e);
  process.exit(1);
}
