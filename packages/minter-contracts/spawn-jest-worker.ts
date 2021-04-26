import yargs from 'yargs';
import path from 'path';
import glob from 'glob';
import spawn from 'cross-spawn';

const args = yargs
  .option('w', {
    alias: 'num-workers',
    describe: 'Number of parallel workers',
    type: 'number',
    default: 4,
  })
  .option('p', {
    alias: 'test-path',
    describe: 'Path to tests folder',
    type: 'string',
    default: 'test',
  })
  .option('i', {
    alias: 'id',
    describe: 'Worker ID',
    type: 'number',
  })
  .demandOption(['i'])
  .help()
  .argv;

const numWorkers = args.w;
const testPath = args.p;
const workerId = args.i;

const files = glob.sync(path.join(testPath, '**/*.test.ts')).sort();
const filesPerWorker = Math.ceil(files.length / numWorkers);

const start = workerId * filesPerWorker;
if (start >= files.length) {
  process.exit();
}

const workerTarget = files.slice(start, start + filesPerWorker);

spawn('yarn', ['jest', '--runInBand', ...workerTarget], {
  stdio: 'inherit',
  env: process.env,
  cwd: process.cwd(),
})
  .on('error', err => {
    throw err;
  })
  .on('exit', code => {
    process.exit(code ?? 0);
  });
