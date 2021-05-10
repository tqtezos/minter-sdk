import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, address, bytes, nat } from '../src/type-aliases';

import {
  originateNftFaucet,
  originateFtFaucet,
  originateFixedPriceSale,
  mintNftTokens,
  mintFtTokens,
  createFtToken,
} from '../src/nft-contracts';
import {
  addOperator,
} from '../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens, getBalances } from './fa2-balance-inspector';

jest.setTimeout(360000); // 6 minutes

describe.each([originateFixedPriceSale])
('marketplace test', (originateMarketplace) => {
  let tezos: TestTz;
  let nft: Contract;
  let ft: Contract;
  let saleFt: Contract;
  let queryBalances: QueryBalances;
  let marketplace: Contract;
  let marketplaceAlice: Contract;
  let marketAddress: address;
  let bobAddress: address;
  let aliceAddress: address;
  let ftTokenId: BigNumber;
  let nftTokenId: BigNumber;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let saleId : nat;
  let saleFtTokenId: nat;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    saleId = new BigNumber(0);
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    nftTokenId = new BigNumber(0);
    ftTokenId = new BigNumber(5);
    saleFtTokenId = new BigNumber(3);
    tokenMetadata = new MichelsonMap();
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(tezos.bob);
    ft = await originateFtFaucet(tezos.bob);
    marketplace = await originateMarketplace(tezos.bob);
    marketplaceAlice = await tezos.alice.contract.at(marketplace.address);
    marketAddress = marketplace.address;
  });

  test('bob makes sale, and alice buys nft', async () => {
    const tokenAmount = new BigNumber(1);
    await createFtToken(tezos.alice, ft, { token_id : ftTokenId, token_info: tokenMetadata });
    await mintFtTokens(tezos.alice, ft, [
      {
        token_id: ftTokenId,
        owner: aliceAddress,
        amount: new BigNumber(1000),
      },
    ]);

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: nftTokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    const [aliceHasNFTTokenBefore, bobHasNFTTokenBefore] = await hasTokens([
      { owner: aliceAddress, token_id: nftTokenId },
      { owner: bobAddress, token_id: nftTokenId },
    ], queryBalances, nft);
    expect(aliceHasNFTTokenBefore).toBe(false);
    expect(bobHasNFTTokenBefore).toBe(true);

    $log.info('making marketplace an operator of alice\'s FT tokens');
    await addOperator(ft.address, tezos.alice, marketAddress, ftTokenId);

    $log.info('making marketplace an operator of bob\'s NFT token');
    await addOperator(nft.address, tezos.bob, marketAddress, nftTokenId);

    $log.info('starting sale...');

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = await marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 0 });
    $log.info(`Waiting for ${buyOp.hash} to be confirmed...`);
    const buyOpHash = await buyOp.confirmation().then(() => buyOp.hash);
    $log.info(`Operation injected at hash=${buyOpHash}`);
  });

  test('bob makes sale, cancels it, then alice unsuccessfully tries to buy', async () => {
    const tokenAmount = new BigNumber(1);
    await createFtToken(tezos.alice, ft, { token_id : ftTokenId, token_info: tokenMetadata });
    await mintFtTokens(tezos.alice, ft, [
      {

        token_id: ftTokenId,
        owner: aliceAddress,
        amount: new BigNumber(1000),
      },
    ]);

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: nftTokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    $log.info('making marketplace an operator of bob\'s token');
    await addOperator(nft.address, tezos.bob, marketAddress, nftTokenId);

    $log.info('making marketplace an operator of alice\'s FT tokens');
    await addOperator(ft.address, tezos.alice, marketAddress, ftTokenId);

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info('bob cancels sale');
    const removeSaleOp = await marketplace.methods
      .cancel(saleId)
      .send({ amount: 0 });
    $log.info(`Waiting for ${removeSaleOp.hash} to be confirmed...`);
    const removeSaleOpHash = await removeSaleOp.confirmation().then(() => removeSaleOp.hash);
    $log.info(`Operation injected at hash=${removeSaleOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 0 });
    expect(buyOp).rejects.toHaveProperty('message', 'NO_SALE');
    $log.info(`alice cannot buy`);

  });

  test('bob makes sale of two fts, and alice buys one', async () => {
    const tokenAmount = new BigNumber(2);
    saleFt = await originateFtFaucet(tezos.bob);
    await createFtToken(tezos.alice, ft, { token_id : ftTokenId, token_info: tokenMetadata });
    await createFtToken(tezos.bob, saleFt, { token_id : saleFtTokenId, token_info: tokenMetadata });

    await mintFtTokens(tezos.alice, ft, [
      {
        token_id: ftTokenId,
        owner: aliceAddress,
        amount: new BigNumber(1000),
      },
    ]);

    await mintFtTokens(tezos.bob, saleFt, [
      {
        token_id: saleFtTokenId,
        owner: bobAddress,
        amount: new BigNumber(1000),
      },
    ]);

    $log.info('making marketplace an operator of alice\'s FT tokens');
    await addOperator(ft.address, tezos.alice, marketAddress, ftTokenId);

    $log.info('making marketplace an operator of bob\'s FT token');
    await addOperator(saleFt.address, tezos.bob, marketAddress, saleFtTokenId);

    $log.info('starting sale...');

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), saleFt.address, saleFtTokenId, ft.address, ftTokenId, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = await marketplaceAlice.methods
      .buy(saleId)
      .send({ amount: 0 });
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
  });


});
