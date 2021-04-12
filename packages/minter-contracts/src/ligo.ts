import * as child from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import { $log } from '@tsed/logger';

import { TezosToolkit } from '@taquito/taquito';
import { Contract } from './type-aliases';

export class LigoEnv {
  readonly cwd: string;
  readonly srcDir: string;
  readonly outDir: string;

  constructor(cwd: string, srcDir: string, outDir: string) {
    this.cwd = cwd;
    this.srcDir = srcDir;
    this.outDir = outDir;
  }

  srcFilePath(srcFileName: string): string {
    return path.join(this.srcDir, srcFileName);
  }

  outFilePath(outFileName: string): string {
    return path.join(this.outDir, outFileName);
  }
}

const ligoVersion  = '0.12.0';
export const ligoImage : string = 'docker run --rm -v $PWD:$PWD -w $PWD ligolang/ligo:' + ligoVersion;
export const defaultEnv: LigoEnv = defaultLigoEnv();

function defaultLigoEnv(): LigoEnv {
  const cwd = path.join(__dirname, '..');
  const src = path.join(cwd, 'ligo/src');
  const out = path.join(cwd, 'bin');
  return new LigoEnv(cwd, src, out);
}

export async function compileAndLoadContract(
  env: LigoEnv,
  srcFile: string,
  main: string,
  dstFile: string,
): Promise<string> {
  const src = env.srcFilePath(srcFile);
  const out = env.outFilePath(dstFile);
  await compileContractImpl(env.cwd, src, main, out);
  return loadFile(out);
}

export async function loadFile(fileName: string): Promise<string> {
  return new Promise<string>((resolve, reject) =>
    fs.readFile(fileName, (err, buff) =>
      err ? reject(err) : resolve(buff.toString()),
    ),
  );
}

export function compileContract(
  env: LigoEnv,
  srcFile: string,
  main: string,
  dstFile: string,
): Promise<void> {
  const src = env.srcFilePath(srcFile);
  const out = env.outFilePath(dstFile);
  return compileContractImpl(env.cwd, src, main, out);
}

function compileContractImpl(
  cwd: string,
  srcFilePath: string,
  main: string,
  dstFilePath: string,
): Promise<void> {
  const cmd = `${ligoImage} compile-contract ${srcFilePath} ${main} --output=${dstFilePath}`;
  return runCmd(cwd, cmd);
}

export function compileLigoExpression(
  env: LigoEnv,
  srcFile: string,
  main: string,
  dstFile: string,
): Promise<void> {
  const src = env.srcFilePath(srcFile);
  const out = env.outFilePath(dstFile);
  return compileLigoExpressionImpl(env.cwd, src, main, out);
}

function compileLigoExpressionImpl(
  cwd: string,
  srcFilePath: string,
  main: string,
  dstFilePath: string,
): Promise<void> {
  const cmd = `${ligoImage} compile-expression --init-file ${srcFilePath} cameligo ${main} | tr -s '\n' ' ' >| ${dstFilePath}`;
  return runCmd(cwd, cmd);
}

export async function runCmd(cwd: string, cmd: string): Promise<void> {
  // const shell = "/bin/zsh";
  return new Promise<void>((resolve, reject) =>
    child.exec(cmd, { cwd }, (err, stdout, errout) => {
      if (stdout) {
        $log.info(stdout);
      }
      if (errout) {
        $log.error(errout);
      }
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    }),
  );
}

export async function originateContract(
  tz: TezosToolkit,
  code: string,
  storage: string | Record<string, any>,
  name: string,
): Promise<Contract> {
  try {
    const originationOp = await tz.contract.originate({
      code: code,
      init: storage,
    });

    const contract = await originationOp.contract();
    $log.info(`originated contract ${name} with address ${contract.address}`);
    $log.info(`consumed gas: ${originationOp.consumedGas}`);
    let storageString : string;
    if (typeof storage === "object"){
      storageString = JSON.stringify(storage);
    }
    else {
      storageString = storage;
    }
    $log.info(`storage initialized to: ${storageString}`);
    return Promise.resolve(contract);
  } catch (error) {
    const jsonError = JSON.stringify(error, null, 2);
    $log.fatal(`${name} origination error ${jsonError}`);
    return Promise.reject(error);
  }
}
