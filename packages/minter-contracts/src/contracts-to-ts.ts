import path from 'path';
import fsRaw from 'fs';
import { $log } from '@tsed/logger';
import { promisify } from 'util';
import * as child from 'child_process';
import net from 'net';

const fs = {
  readdir: promisify(fsRaw.readdir),
  readFile: promisify(fsRaw.readFile),
  writeFile: promisify(fsRaw.writeFile),
  unlink: promisify(fsRaw.unlink),
};

const binFolder = 'bin';
const binPath = path.resolve(path.join(__dirname, '..', binFolder));
const contracts = fsRaw.readdirSync(binPath);
const outFolder = 'bin-ts';
const outPath = path.resolve(path.join(__dirname, '..', outFolder));
const sandboxPort = 20000;

// TODO: Make this less hacky.
// @taquito/michel-codec isn't reliably up to date with the protocol.
// To leverage a canonical JSON conversion mechanism, we're downloading the
// tezos-client and passing it commands.
const tzClientPath = 'tezos-client';
const ensureTzClient = () => {
  if (!fsRaw.existsSync(tzClientPath)) {
    child.execSync(`wget --quiet -O ${tzClientPath} https://github.com/serokell/tezos-packaging/releases/download/v8.2-3/tezos-client`);
  }

  fsRaw.chmodSync(tzClientPath, '755');
};

const cleanTzClient = () => {
  if (fsRaw.existsSync(tzClientPath)) {
    fsRaw.unlinkSync(tzClientPath);
  }
};

const michelsonToJson = (fileName: string) => new Promise<string>((resolve, reject) => {
  child.exec(`TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=yes ${tzClientPath} convert script ${fileName} from michelson to JSON`, (err, stdout, stderr) => {
    if (stderr) {
      $log.error(stderr);
    }
    err ? reject(err) : resolve(stdout);
  });
});

const testPort = (port: number) => new Promise<void>((resolve, reject) => {
  const socket = new net.Socket();
  socket.connect(port, '0.0.0.0');
  socket.on('error', () => {
    socket.destroy();
    reject(
      `Sandbox server doesn't appear to be running (I checked port ${port}). ` +
      'Please ensure sandbox is running.',
    );
  });
  socket.on('connect', () => {
    socket.destroy();
    resolve();
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

const emptyDirectory = (dir: string) => fs.readdir(dir).then(files => Promise.all(files.map(file =>
  fs.unlink(path.join(dir, file)),
)));

const compile = () =>
  Promise.all(contracts.map(async fileName => {
    try {
      $log.info(`compiling ${fileName} to JSON`);
      // Parse the JSON with JavaScript here so we can later leverage JSON.stringify
      // as a means to "minify" the resulting output. The tezos-client call
      // preserves whitespace which dramatically bloats the output size.
      // This does incur an additional decode->encode cycle in the script, but
      // the resulting file size savings is worth the additional roundtrip.
      const json = JSON.parse(await michelsonToJson(path.join(binPath, fileName)));
      const alias = fileNameToAlias(fileName);
      await fs.writeFile(
        path.join(outPath, `${fileName}.ts`),
        `export type ${alias} = { _type: '${alias}'; code: object; };\n` +
        `export default { _type: '${alias}', code: JSON.parse(\`${JSON.stringify(json)}\`) } as ${alias};\n`,
      );
      $log.info(`successfully compiled ${fileName} to JSON.`);
      return fileName;
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
    await testPort(sandboxPort);
    ensureTzClient();
    await emptyDirectory(outPath);
    const compiledFiles = (await compile()).filter(Boolean) as string[];
    fsRaw.writeFileSync('index.ts', generateIndex(compiledFiles));
  } catch (e) {
    $log.error(e);
  }

  cleanTzClient();
})();
