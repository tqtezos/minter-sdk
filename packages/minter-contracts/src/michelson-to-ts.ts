import path from 'path';
import fs from 'fs';
import { $log } from '@tsed/logger';
import * as child from 'child_process';
import yargs from 'yargs';

const args = yargs
  .option('client-path', { alias: 'c', describe: 'path to tezos-client binary', type: 'string' })
  .option('michelson-path', {
    alias: 'm',
    describe: 'path to compiled Michelson folder',
    default: path.join(__dirname, '..', 'bin'),
    type: 'string',
  })
  .option('out-path', {
    alias: 'o',
    describe: 'TypeScript output path',
    type: 'string',
    default: path.join(__dirname, '..', 'bin-ts'),
  })
  .option('endpoint', {
    alias: 'E',
    description: `HTTP(S) endpoint of the node RPC interface; e.g. 'http://localhost:8732'`,
    type: 'string',
    default: 'http://localhost:20000',
  })
  .demandOption('client-path')
  .help()
  .argv;

const tzClientPath = args['client-path'];
const outPath = args['out-path'];
const michelsonPath = args['michelson-path'];
const endpoint = args['endpoint'];
const michelsonToJson = (fileName: string) => new Promise<string>((resolve, reject) => {
  child.exec(`TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=yes ${tzClientPath} -E ${endpoint} convert script ${fileName} from michelson to JSON`, (err, stdout, stderr) => {
    if (stderr) {
      $log.error(stderr);
    }
    err ? reject(err) : resolve(stdout);
  });
});

const fileNameToTitleCase = (fileName: string) =>
  fileName
    .split('.')[0]
    .split('_')
    .map(x => x[0].toUpperCase() + x.substring(1))
    .join('')
  + 'Code';
const titleCaseToAlias = (titleCaseFileName: string) => `${titleCaseFileName}Type`;
const fileNameToAlias = (fileName: string) => titleCaseToAlias(fileNameToTitleCase(fileName));

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const emptyDirectory = (dir: string, predicate = (_: string) => true) =>
  fs.promises.readdir(dir).then(files => Promise.all(files.filter(predicate).map(file =>
    fs.promises.unlink(path.join(dir, file)),
  )));

const compile = (contracts: string[]) =>
  Promise.all(contracts.map(async fileName => {
    $log.info(`compiling ${fileName} to JSON`);
    // Parse the JSON with JavaScript here so we can later leverage JSON.stringify
    // as a means to "minify" the resulting output. The tezos-client call
    // preserves whitespace which dramatically bloats the output size.
    // This does incur an additional decode->encode cycle in the script, but
    // the resulting file size savings is worth the additional roundtrip.
    const json = JSON.parse(await michelsonToJson(path.join(michelsonPath, fileName)));
    const alias = fileNameToAlias(fileName);
    await fs.promises.writeFile(
      path.join(outPath, `${fileName}.ts`),
      `export type ${alias} = { __type: '${alias}'; code: object; };\n` +
      `export default { __type: '${alias}', code: JSON.parse(\`${JSON.stringify(json)}\`) } as ${alias};\n`,
    );
    $log.info(`successfully compiled ${fileName} to JSON.`);
    return fileName;
  }));

const generateIndex = (compiledFiles: string[]) => {
  const imports = compiledFiles
    .map(fileName =>
      `import ${fileNameToTitleCase(fileName)}, { ${fileNameToAlias(fileName)} } from './${path.relative(process.cwd(), outPath)}/${fileName}';`,
    )
    .join('\n');

  const exports = compiledFiles
    .map(fileName => `  ${fileNameToTitleCase(fileName)},\n  ${fileNameToAlias(fileName)},`)
    .join('\n');

  return `${imports}\n\nexport {\n${exports}\n};\n`;
};

(async () => {
  try {
    const [contracts] = await Promise.all([
      // Retrieve compiled Michelson
      fs.promises
        .readdir(michelsonPath)
        .then(files => files.filter(f => path.extname(f) === '.tz')),
      // Clear any existing .ts files in the output path
      emptyDirectory(outPath, f => path.extname(f) === '.ts'),
    ]);
    const compiledFiles = await compile(contracts);
    $log.info('Writing index');
    fs.writeFileSync('index.ts', generateIndex(compiledFiles));
    process.exit(0);
  } catch (e) {
    $log.error(e);
    process.exit(1);
  }
})();
