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
    .join('');
const fileNameToContractType = (fileName: string) => fileNameToTitleCase(fileName) + 'ContractType';
const fileNameToCodeObject = (fileName: string) => fileNameToTitleCase(fileName) + 'Code';

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const emptyDirectory = (dir: string, predicate = (_: string) => true) =>
  fs.promises.readdir(dir).then(files => Promise.all(files.filter(predicate).map(file =>
    fs.promises.unlink(path.join(dir, file)),
  )));

const compileToJsonFiles = (contracts: string[]) =>
  Promise.all(contracts.map(async fileName => {
    $log.info(`compiling ${fileName} to JSON`);
    // Parse the JSON with JavaScript here so we can later leverage JSON.stringify
    // as a means to "minify" the resulting output. The tezos-client call
    // preserves whitespace which dramatically bloats the output size.
    // This does incur an additional decode->encode cycle in the script, but
    // the resulting file size savings is worth the additional roundtrip.
    const json = JSON.parse(await michelsonToJson(path.join(michelsonPath, fileName)));
    await fs.promises.writeFile(
      path.join(outPath, `${fileName.replace('.tz', '.json')}`),
      JSON.stringify(json),
    );
    $log.info(`successfully compiled ${fileName} to JSON.`);
    return fileName;
  }));

const generateTypes = () => new Promise<string>((resolve, reject) => {
  child.exec(`yarn generate-types`, (err, stdout, stderr) => {
    if (stderr) {
      $log.error(stderr);
    }
    err ? reject(err) : resolve(stdout);
  });
});

const generateIndex = (codeFiles: string[]) => {
  const fileNames = codeFiles
    .map(x => x.replace('.code.ts', ''));

  const codeImports = fileNames
    .map(fileName =>
      `import { ${fileNameToCodeObject(fileName)} } from './${path.relative(process.cwd(), outPath)}/${fileName}.code';\n` +
      `import { ${fileNameToContractType(fileName)} } from './${path.relative(process.cwd(), outPath)}/${fileName}.types';`
      ,
    )
    .join('\n');

  const exports = codeFiles
    .map(fileName => `  ${fileNameToContractType(fileName)},\n  ${fileNameToCodeObject(fileName)},`)
    .join('\n');

  return `${codeImports}\n\nexport {\n${exports}\n};\n`;
};

(async () => {
  try {
    fs.promises.mkdir(outPath);
    const [contracts] = await Promise.all([
      // Retrieve compiled Michelson
      fs.promises
        .readdir(michelsonPath)
        .then(files => files.filter(f => path.extname(f) === '.tz')),
      // Clear any existing .json and .ts files in the output path
      emptyDirectory(outPath, f => path.extname(f) === '.json'),
      emptyDirectory(outPath, f => path.extname(f) === '.ts'),
    ]);

    // Compile json
    await compileToJsonFiles(contracts);

    // Generate Types
    await generateTypes();

    const outputFiles = await fs.promises.readdir(outPath);
    const codeFiles = outputFiles
      .filter(x => x.endsWith('.code.ts'));

    $log.info('Writing index');
    fs.writeFileSync('index.ts', generateIndex(codeFiles));
    process.exit(0);
  } catch (e) {
    $log.error(e);
    process.exit(1);
  }
})();
