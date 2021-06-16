import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';

import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, address, bytes, nat } from '../../src/type-aliases';

import {
  originateNftFaucet,
  originateFtFaucet,
  mintNftTokens,
  originateFixedPriceAdminSale,
} from '../../src/nft-contracts';
import {
  addOperator,
} from '../../src/fa2-interface';
import { queryBalancesWithLambdaView, QueryBalances, hasTokens } from '../fa2-balance-inspector';
import { createPurchaseToken } from '../../src/fixed-price';

jest.setTimeout(360000); // 6 minutes


describe.each([originateFixedPriceAdminSale])
('marketplace test', (originateMarketplace) => {
  let tezos: TestTz;
  let nft: Contract;
  let ft: Contract;
  let marketplace: Contract;
  let marketAddress: address;
  let bobAddress: address;
  let aliceAddress: address;
  let ftTokenId: BigNumber;
  let nftTokenId: BigNumber;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let marketplaceAlice: Contract;
  let saleId: nat;
  let initFtBalance: nat;

  let queryBalances: QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    nftTokenId = new BigNumber(0);
    ftTokenId = new BigNumber(5);
    tokenMetadata = new MichelsonMap();
    saleId = new BigNumber(0);
    initFtBalance = new BigNumber(1000);
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(tezos.eve);
    ft = await originateFtFaucet(tezos.eve);
    marketplace = await originateMarketplace(tezos.bob, bobAddress);
    marketAddress = marketplace.address;
    marketplaceAlice = await tezos.alice.contract.at(marketAddress);
  });

  test('bob makes sale, and alice buys nft', async () => {
    const tokenAmount = new BigNumber(1);
    await createPurchaseToken(marketplace.address, ft, ft.address, ftTokenId,
      tezos.alice, aliceAddress, tokenMetadata, initFtBalance);

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

    $log.info('making marketplace an operator of bob\'s NFT token');
    await addOperator(nft.address, tezos.bob, marketAddress, nftTokenId);

    $log.info('starting sale...');

    $log.info('pause marketplace');
    const pauseOp = await marketplace.methods.pause(true).send({ amount: 0 });
    $log.info(`Waiting for ${pauseOp.hash} to be confirmed...`);
    const pauseOpHash = await pauseOp.confirmation().then(() => pauseOp.hash);
    $log.info(`Operation injected at hash=${pauseOpHash}`);

    $log.info(`Attempting to create sale while contract is paused`);
    const pausedSellOp = marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount).send({ amount: 0 });
    expect(pausedSellOp).rejects.toHaveProperty('message', 'PAUSED');

    $log.info('unpause marketplace');
    const unpauseOp = await marketplace.methods.pause(false).send({ amount: 0 });
    $log.info(`Waiting for ${unpauseOp.hash} to be confirmed...`);
    const unpauseOpHash = await unpauseOp.confirmation().then(() => unpauseOp.hash);
    $log.info(`Operation injected at hash=${unpauseOpHash}`);

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount).send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = await marketplaceAlice.methods
      .buy({
        sale_id : saleId,
        buy_amount : new BigNumber(1),
      })
      .send({ amount: 0 });
    $log.info(`Waiting for ${buyOp.hash} to be confirmed...`);
    const buyOpHash = await buyOp.confirmation().then(() => buyOp.hash);
    $log.info(`Operation injected at hash=${buyOpHash}`);

  });

  test('bob makes sale, cancels it, then alice unsuccessfully tries to buy', async () => {
    const tokenAmount = new BigNumber(1);

    await createPurchaseToken(marketplace.address, ft, ft.address, ftTokenId,
      tezos.alice, aliceAddress, tokenMetadata, initFtBalance);

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

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);

    $log.info('alice cancels sale (not admin nor seller)');
    const cancelOp = marketplaceAlice.methods
      .cancel(saleId).send({ amount: 0 });
    expect(cancelOp).rejects.toHaveProperty('message', 'NOT_AN_ADMIN_OR_A_SELLER');

    $log.info('bob cancels sale');
    const removeSaleOp = await marketplace.methods
      .cancel(saleId).send({ amount: 0 });
    $log.info(`Waiting for ${removeSaleOp.hash} to be confirmed...`);
    const removeSaleOpHash = await removeSaleOp.confirmation().then(() => removeSaleOp.hash);
    $log.info(`Operation injected at hash=${removeSaleOpHash}`);

    $log.info(`Alice buys non-fungible token with her fungible tokens`);
    const buyOp = marketplaceAlice.methods
      .buy({
        sale_id : saleId,
        buy_amount : new BigNumber(1),
      })
      .send({ amount: 0 });
    expect(buyOp).rejects.toHaveProperty('message', 'NO_SALE');
    $log.info(`alice cannot buy`);
  });
});
