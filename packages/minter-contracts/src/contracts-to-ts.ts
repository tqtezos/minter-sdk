import path from 'path';
import fsRaw from 'fs';
import { Parser } from '@taquito/michel-codec';
import { $log } from '@tsed/logger';
import { promisify } from 'util';

const fs = {
  readdir: promisify(fsRaw.readdir),
  readFile: promisify(fsRaw.readFile),
  writeFile: promisify(fsRaw.writeFile),
  unlink: promisify(fsRaw.unlink),
};

const binFolder = 'bin';
const binPath = path.resolve(path.join(__dirname, '..', binFolder));
const contracts = fsRaw.readdirSync(binPath);
const parser = new Parser();
const outFolder = 'bin-ts';
const outPath = path.resolve(path.join(__dirname, '..', outFolder));

const fileNameToTitleCase = (fileName: string) =>
  fileName
    .split('.')[0]
    .split("_")
    .map(x => x[0].toUpperCase() + x.substring(1))
    .join('')
  + "Code";
const titleCaseToAlias = (titleCaseFileName: string) => `${titleCaseFileName}Type`;
const fileNameToAlias = (fileName: string) => titleCaseToAlias(fileNameToTitleCase(fileName));

const emptyDirectory = (dir: string) => fs.readdir(dir).then(files => Promise.all(files.map(file =>
  fs.unlink(path.join(dir, file)),
)));

const compile = () =>
  Promise.all(contracts.map(async fileName => {
    try {
      $log.info(`compiling ${fileName} to TypeScript AST`);
      const contents = await fs.readFile(path.join(binPath, fileName), 'utf8');
      const parsed = parser.parseScript(contents);
      const alias = fileNameToAlias(fileName);
      return await fs.writeFile(
        path.join(outPath, `${fileName}.ts`),
        `export type ${alias} = { _type: "${alias}"; code: object; };\n` +
        `export default { _type: '${alias}', code: JSON.parse(\`${JSON.stringify(parsed)}\`) } as ${alias};\n`,
      ).then(() => fileName);
    } catch (e) {
      $log.warn(`Error parsing ${fileName}: ${e}`);
    }
  }));

const generateIndex = (compiledFiles: string[]) => {
  const imports = compiledFiles
    .map(fileName =>
      `import ${fileNameToTitleCase(fileName)}, { ${fileNameToAlias(fileName)} } from './${outFolder}/${fileName}';`,
    )
    .join('\n');

  const exports = compiledFiles
    .map(fileName => `  ${fileNameToTitleCase(fileName)},\n  ${fileNameToAlias(fileName)},`)
    .join('\n');

  return `${imports}\n\nexport {\n${exports}\n};\n`;
};

(async () => {
  try {
    await emptyDirectory(outPath);
    const compiledFiles = (await compile()).filter(Boolean) as string[];
    fsRaw.writeFileSync('index.ts', generateIndex(compiledFiles));
  } catch (e) {
    $log.error(e);
  }
})();
