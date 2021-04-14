import { TezosToolkit, VIEW_LAMBDA } from '@taquito/taquito';
import { Signer } from '@taquito/taquito/dist/types/signer/interface';
import { InMemorySigner } from '@taquito/signer';
import { $log } from '@tsed/logger';
import retry from 'async-retry';

type TestKeys = {
    bob: Signer;
    alice: Signer;
    lambdaView?: string;
};

async function flextesaKeys(): Promise<TestKeys> {
  const bob = await InMemorySigner.fromSecretKey(
    'edsk3RFgDiCt7tWB2oe96w1eRw72iYiiqZPLu9nnEY23MYRp2d8Kkx',
  );
  const alice = await InMemorySigner.fromSecretKey(
    'edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq',
  );
  return { bob, alice };
}

async function testnetKeys(): Promise<TestKeys> {
  const bob = await InMemorySigner.fromSecretKey(
    'edskRfLsHb49bP4dTpYzAZ7qHCX4ByK2g6Cwq2LWqRYAQSeRpziaZGBW72vrJnp1ahLGKd9rXUf7RHzm8EmyPgseUi3VS9putT',
  );
  const alice = await InMemorySigner.fromSecretKey(
    'edskRqb8GgnD4d2B7nR3ofJajDU7kwooUzXz7yMwRdLDP9j7Z1DvhaeBcs8WkJ4ELXXJgVkq5tGwrFibojDjYVaG7n4Tq1qDxZ',
  );
  return { bob, alice };
}

export type TestTz = {
    bob: TezosToolkit;
    alice: TezosToolkit;
    lambdaView?: string;
};

export type TestTzMarket = {
    bob: TezosToolkit;
    alice: TezosToolkit;
    market: TezosToolkit;
};

function signerToToolkit(signer: Signer, rpc: string): TezosToolkit {
  const tezos = new TezosToolkit(rpc) as TezosToolkit;
  tezos.setProvider({
    signer,
    rpc,
    config: { confirmationPollingIntervalSecond: 3 },
  });
  return tezos;
}

export async function awaitForNetwork(tz: TezosToolkit): Promise<void> {
  await retry(
    async () => {
      $log.info('connecting to Tezos network...');
      await tz.rpc.getBlockHeader({ block: '2' });
    },
    { retries: 8 },
  );

  $log.info('connected to Tezos network');
}

export async function bootstrap(): Promise<TestTz> {
  const { bob, alice } = await flextesaKeys();
  const rpc = 'http://localhost:20000';
  const bobToolkit = signerToToolkit(bob, rpc);
  const aliceToolkit = signerToToolkit(alice, rpc);

  await awaitForNetwork(bobToolkit);

  $log.info('originating lambda view contract...');
  const op = await bobToolkit.contract.originate({
    code: VIEW_LAMBDA.code,
    storage: VIEW_LAMBDA.storage,
  });
  const lambdaContract = await op.contract();
  $log.info('originated lambda view contract.');
  return {
    bob: bobToolkit,
    alice: aliceToolkit,
    lambdaView: lambdaContract.address,
  };
}

export async function adminBootstrap(): Promise<TezosToolkit> {
  // SECRET MAY HAVE TO BE UPDATED
  const secret = 'edsk3g1kc8UzJJhZn6kTecW6vb6m1qnaWXYDFahGHqcmLbepUT3pFe';
  const adminSigner = await InMemorySigner.fromSecretKey(secret);
  const rpc = 'http://localhost:20000';
  return signerToToolkit(adminSigner, rpc);
}

export async function bootstrapTestnet(): Promise<TestTz> {
  const { bob, alice } = await testnetKeys();
  const rpc = 'https://testnet-tezos.giganode.io';
  return {
    bob: signerToToolkit(bob, rpc),
    alice: signerToToolkit(alice, rpc),
  };
}
