import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { TezosToolkit, MichelsonMap } from '@taquito/taquito';

import { adminBootstrap, bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, address, bytes } from '../src/type-aliases';

import {
  originateNftFaucet,
  MintNftParam,
  originateFixedPriceTezAdminSale,
} from '../src/nft-contracts';
import {
  BalanceOfRequest,
  addOperator,
} from '../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView } from './fa2-balance-inspector';

jest.setTimeout(180000); // 3 minutes

describe.each([originateFixedPriceTezAdminSale])
('marketplace test', (originateMarketplace) => {
  let tezos: TestTz;
  let nft: Contract;
  let queryBalances: QueryBalances;
  let marketplace: Contract;
  let marketAddress: address;
  let bobAddress: address;
  let aliceAddress: address;
  let tokenId: BigNumber;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let salePrice: BigNumber;
  let adminAddress: address;
  let adminToolkit: TezosToolkit;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    adminToolkit = await adminBootstrap();
    adminAddress = await adminToolkit.signer.publicKeyHash();
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(adminToolkit, adminAddress);
    marketplace = await originateMarketplace(tezos.bob, adminAddress);
    marketAddress = marketplace.address;
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    tokenId = new BigNumber(0);
    tokenMetadata = new MichelsonMap();
    salePrice = new BigNumber(1000000);
  });

  async function hasTokens(requests: BalanceOfRequest[]): Promise<boolean[]> {
    const responses = await queryBalances(nft, requests);
    const results = responses.map(r => {
      if (r.balance.eq(1)) return true;
      else if (r.balance.eq(0)) return false;
      else throw new Error(`Invalid NFT balance ${r.balance}`);
    });
    return results;
  }

  async function mintTokens(
    tz: TezosToolkit,
    tokens: MintNftParam[],
  ): Promise<void> {
    $log.info('minting...');
    const op = await nft.methods.mint(tokens).send();
    await op.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${op.consumedGas}`);
  }


  test('bob makes sale, and alice buys nft', async () => {

    try {

      await mintTokens(tezos.bob, [
        {
          token_metadata: {
            token_id: tokenId,
            token_info: tokenMetadata,
          },
          owner: bobAddress,
        },
      ]);

      const [aliceHasATokenBefore, bobHasATokenBefore] = await hasTokens([
        { owner: aliceAddress, token_id: tokenId },
        { owner: bobAddress, token_id: tokenId },
      ]);
      expect(aliceHasATokenBefore).toBe(false);
      expect(bobHasATokenBefore).toBe(true);

      $log.info('making marketplace an operator of bob\'s token');
      await addOperator(nft.address, tezos.bob, marketAddress, tokenId);

      const storage:any = await nft.storage();
      const assets:any = await storage.assets;
      const operators:any = await assets.operators;
      $log.info(`operators ${JSON.stringify(operators, null, 2)}`);
      // const assets = await storage.assets;

      // const ret = await assets.operators;

      // const retIterator = ret.keys();

      // $log.info(`storage.assets: ${JSON.stringify(assets,null,2)}`);
      // $log.info(retIterator.next());

      $log.info('starting sale...');

      $log.info('pause marketplace');
      const pauseOp = await marketplace.methods.pause(true).send({ amount: 0 });
      $log.info(`Waiting for ${pauseOp.hash} to be confirmed...`);
      const pauseOpHash = await pauseOp.confirmation(1).then(() => pauseOp.hash);
      $log.info(`Operation injected at hash=${pauseOpHash}`);

      try
      {
        $log.info(`Attempting to create sale while contract is paused`);
        await marketplace.methods.sell(salePrice, nft.address, tokenId).send({ source: bobAddress, amount: 0 });
      } catch (error) {$log.info(`Confirmation: Cannot create sale while contract is paused`);}

      $log.info('unpause marketplace');
      const unpauseOp = await marketplace.methods.pause(false).send({ amount: 0 });
      $log.info(`Waiting for ${unpauseOp.hash} to be confirmed...`);
      const unpauseOpHash = await unpauseOp.confirmation(1).then(() => unpauseOp.hash);
      $log.info(`Operation injected at hash=${unpauseOpHash}`);

      $log.info(`Creating sale`);
      const sellOp = await marketplace.methods
        .sell(salePrice, nft.address, tokenId)
        .send({ source: bobAddress, amount: 0 });
      $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
      const sellOpHash = await sellOp.confirmation(1).then(() => sellOp.hash);
      $log.info(`Operation injected at hash=${sellOpHash}`);
      $log.info('alice buys nft...');


      // const aliceSaleContract = await tezos.alice.contract.at(marketplace.address);
      const buyOp = await marketplace.methods
        .buy(bobAddress, nft.address, tokenId)
        .send({ source: aliceAddress, amount: 1 });
      $log.info(`Waiting for ${buyOp.hash} to be confirmed...`);
      const buyOpHash = await buyOp.confirmation().then(() => buyOp.hash);
      $log.info(`Operation injected at hash=${buyOpHash}`);

    } catch (error) {
      $log.info(`Error: ${JSON.stringify(error, null, 2)}`);
    }

  });

  test('bob makes sale, cancels it, then alice unsuccessfully tries to buy', async () => {

    await mintTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: tokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ]);

    const [aliceHasATokenBefore, bobHasATokenBefore] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
      { owner: bobAddress, token_id: tokenId },
    ]);
    expect(aliceHasATokenBefore).toBe(false);
    expect(bobHasATokenBefore).toBe(true);

    $log.info('making marketplace an operator of bob\'s token');
    await addOperator(nft.address, tezos.bob, marketAddress, tokenId);

    try {

      $log.info('starting sale...');
      const sellOp = await marketplace.methods
        .sell(salePrice, nft.address, tokenId)
        .send({ source: bobAddress, amount: 0 });
      $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
      const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
      $log.info(`Operation injected at hash=${sellOpHash}`);

      try
      {
        $log.info('alice cancels sale (not admin nor seller)');
        await marketplace.methods.cancel(salePrice, nft.address, tokenId).send({ source: aliceAddress, amount: 0 });
      } catch (error) {
        $log.info(`Alice cannot cancel sale, since she is not an admin`);
      }

      $log.info('bob cancels sale');
      const removeSaleOp = await marketplace.methods
        .cancel(salePrice, nft.address, tokenId)
        .send({ source: bobAddress, amount: 0 });
      $log.info(`Waiting for ${removeSaleOp.hash} to be confirmed...`);
      const removeSaleOpHash = await removeSaleOp.confirmation(1).then(() => removeSaleOp.hash);
      $log.info(`Operation injected at hash=${removeSaleOpHash}`);
      $log.info(`alice tries to buy`);
      await marketplace.methods.buy(bobAddress, nft.address, tokenId).send({ source: aliceAddress, amount: 1 });
    } catch (error) {
      $log.info(`alice cannot buy`);
    }

  });

});
