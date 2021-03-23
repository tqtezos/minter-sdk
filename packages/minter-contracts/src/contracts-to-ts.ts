import path from 'path';
import fs from 'fs';
import { Parser } from '@taquito/michel-codec';
import { $log } from '@tsed/logger';

const binFolder = 'bin';
const binPath = path.resolve(path.join(__dirname, '..', binFolder));
const contracts = fs.readdirSync(binPath);
const parser = new Parser();
const outFolder = 'bin-ts';
const outPath = path.resolve(path.join(__dirname, '..', outFolder));
const aliasFile = 'type-aliases';

const fileNameToTitleCase = (fileName: string) =>
  fileName
    .split('.')[0]
    .split("_")
    .map(x => x[0].toUpperCase() + x.substring(1))
    .join('')
  + "Code";
const titleCaseToAlias = (titleCaseFileName: string) => `${titleCaseFileName}Type`;
const fileNameToAlias = (fileName: string) => titleCaseToAlias(fileNameToTitleCase(fileName));

const emptyDirectory = (dir: string) => new Promise((resolve,reject) => fs.readdir(dir, (err, files) =>
  err ? reject(err) : resolve(Promise.all(files.map(file =>
    new Promise<void>((resolve, reject) => fs.unlink(path.join(dir, file), (err) =>
      err ? reject(err) : resolve(),
    )),
  ))),
));

const writeFileP = (path: number | fs.PathLike, data: any) =>
  new Promise<void>((resolve, reject) =>
    fs.writeFile(path, data, (err) => err ? reject(err) : resolve()));

const compile = () =>
  Promise.all(contracts.map(async fileName => {
    try {
      $log.info(`compiling ${fileName} to TypeScript AST`);
      const contents = await new Promise<string>((resolve, reject) =>
        fs.readFile(path.join(binPath, fileName), 'utf8', (err, contents) =>
          err ? reject(err) : resolve(contents),
        ),
      );
      const parsed = parser.parseScript(contents);
      const alias = fileNameToAlias(fileName);
      return await writeFileP(
        path.join(outPath, `${fileName}.ts`),
        `import { ${alias} } from './${aliasFile}';\n` +
        `export default { _type: '${alias}', code: JSON.parse(\`${JSON.stringify(parsed)}\`) } as ${alias};\n`,
      )
        .then(() => fileName);
    } catch (e) {
      $log.warn(`Error parsing ${fileName}: ${e}`);
    }
  }));

const generateIndex = (compiledFiles: string[]) => {
  const imports = compiledFiles
    .map(fileName => `import ${fileNameToTitleCase(fileName)} from './${outFolder}/${fileName}';`)
    .join('\n');

  const exports = compiledFiles
    .map(fileName => `  ${fileNameToTitleCase(fileName)},`)
    .join('\n');

  return `${imports}\n\nexport * from './${outFolder}/${aliasFile}';\n\nexport {\n${exports}\n};\n`;
};

const generateTypeAliases = (compiledFiles: string[]) =>
  compiledFiles
    .map(fileName => {
      const alias = fileNameToAlias(fileName);
      return `export type ${alias} = { _type: "${alias}"; code: object };`;
    })
    .join('\n')
    + '\n';

(async () => {
  try {
    await emptyDirectory(outPath);
    const compiledFiles = (await compile()).filter(Boolean) as string[];
    await Promise.all([
      writeFileP('index.ts', generateIndex(compiledFiles)),
      writeFileP(path.join(outPath, `${aliasFile}.ts`), generateTypeAliases(compiledFiles)),
    ]);
  } catch (e) {
    $log.error(e);
  }
})();
