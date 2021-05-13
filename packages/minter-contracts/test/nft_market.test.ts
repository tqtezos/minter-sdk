import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, address, bytes, nat } from '../src/type-aliases';

import {
  originateNftFaucet,
  originateFixedPriceTezSale,
  createFtToken,
  mintFtTokens,
  mintNftTokens,
  originateFtFaucet,
} from '../src/nft-contracts';
import {
  addOperator,
} from '../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens, getBalances } from './fa2-balance-inspector';

jest.setTimeout(300000); // 5 minutes

describe.each([originateFixedPriceTezSale])
('marketplace test', (originateMarketplace) => {
  let tezos: TestTz;
  let nft: Contract;
  let queryBalances: QueryBalances;
  let marketplace: Contract;
  let marketplaceAlice : Contract;
  let marketAddress: address;
  let bobAddress: address;
  let aliceAddress: address;
  let tokenId: nat;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let salePrice: nat;
  let saleId : nat;
  let saleFtTokenId: nat;
  let saleFt: Contract;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
  });

  beforeEach(async () => {
    await tezos.bob.signer.publicKeyHash();
    nft = await originateNftFaucet(tezos.bob);
    marketplace = await originateMarketplace(tezos.bob);
    marketAddress = marketplace.address;
    marketplaceAlice = await tezos.alice.contract.at(marketAddress);
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    tokenId = new BigNumber(0);
    tokenMetadata = new MichelsonMap();
    salePrice = new BigNumber(1000000);
    saleId = new BigNumber(0);
    saleFtTokenId = new BigNumber(0);
  });

  test('bob makes sale, and alice buys nft', async () => {
    const tokenAmount = new BigNumber(1);

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: tokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    const [aliceHasATokenBefore, bobHasATokenBefore] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
      { owner: bobAddress, token_id: tokenId },
    ], queryBalances, nft);
    expect(aliceHasATokenBefore).toBe(false);
    expect(bobHasATokenBefore).toBe(true);

    $log.info('making marketplace an operator of bob\'s token');
    await addOperator(nft.address, tezos.bob, marketAddress, tokenId);

    $log.info('starting sale...');
    const bobSaleContract = await tezos.bob.contract.at(marketplace.address);
    const sellOp = await bobSaleContract.methods
      .sell(nft.address, tokenId, salePrice, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation(1).then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);
    $log.info('alice buys nft...');
    const buyOp = await marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 1 });
    $log.info(`Waiting for ${buyOp.hash} to be confirmed...`);
    const buyOpHash = await buyOp.confirmation().then(() => buyOp.hash);
    $log.info(`Operation injected at hash=${buyOpHash}`);
  });

  test('bob makes sale, cancels it, then alice unsuccessfully tries to buy', async () => {
    const tokenAmount = new BigNumber(1);

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: tokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    const [aliceHasATokenBefore, bobHasATokenBefore] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
      { owner: bobAddress, token_id: tokenId },
    ], queryBalances, nft);
    expect(aliceHasATokenBefore).toBe(false);
    expect(bobHasATokenBefore).toBe(true);

    $log.info('making marketplace an operator of bob\'s token');
    await addOperator(nft.address, tezos.bob, marketAddress, tokenId);

    $log.info('starting sale...');
    const bobSaleContract = await tezos.bob.contract.at(marketplace.address);
    const sellOp = await bobSaleContract.methods
      .sell(nft.address, tokenId, salePrice, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation(1).then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);
    $log.info('bob cancels sale');
    const removeSaleOp = await bobSaleContract.methods
      .cancel(saleId)
      .send({ amount: 0 });
    $log.info(`Waiting for ${removeSaleOp.hash} to be confirmed...`);
    const removeSaleOpHash = await removeSaleOp.confirmation(1).then(() => removeSaleOp.hash);
    $log.info(`Operation injected at hash=${removeSaleOpHash}`);
    $log.info(`alice tries to buy`);
    const buyOp = marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 1 });
    expect(buyOp).rejects.toHaveProperty('message', 'NO_SALE');

  });

  test('bob makes sale of two fts, and alice buys one', async () => {
    const tokenAmount = new BigNumber(2);
    saleFt = await originateFtFaucet(tezos.bob);

    await createFtToken(tezos.bob, saleFt, { token_id : saleFtTokenId, token_info: tokenMetadata });

    await mintFtTokens(tezos.bob, saleFt, [
      {
        token_id: saleFtTokenId,
        owner: bobAddress,
        amount: new BigNumber(1000),
      },
    ]);

    $log.info('making marketplace an operator of bob\'s FT token');
    await addOperator(saleFt.address, tezos.bob, marketAddress, saleFtTokenId);

    $log.info('starting sale...');

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(saleFt.address, saleFtTokenId, salePrice, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = await marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 1 });
    $log.info(`Waiting for ${buyOp.hash} to be confirmed...`);
    const buyOpHash = await buyOp.confirmation().then(() => buyOp.hash);
    $log.info(`Operation injected at hash=${buyOpHash}`);

    const [aliceBalanceSaleFt] = await getBalances([
      { owner: aliceAddress, token_id: saleFtTokenId },
    ], queryBalances, saleFt);

    expect(aliceBalanceSaleFt.isEqualTo(1)).toBe(true);
    const marketplaceStorage: any = await marketplace.storage();
    const sale : any = await marketplaceStorage.sales.get('0');
    const amount : any = await sale.sale_data.amount;
    expect(JSON.stringify(amount, null, 2)).toEqual("\"1\"");
    $log.info("Amount of token left for sale decremented as expected");
  });

});
