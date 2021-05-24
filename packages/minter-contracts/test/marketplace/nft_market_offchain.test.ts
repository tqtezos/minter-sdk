import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';

import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, address, bytes, nat } from '../../src/type-aliases';

import {
  originateNftFaucet,
  originateFixedPriceTezSale,
  createFtToken,
  mintFtTokens,
  mintNftTokens,
  originateFtFaucet,
  originateFixedPriceTezOffchainSale,
} from '../../src/nft-contracts';
import {
  addOperator,
} from '../../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens, getBalances } from '../fa2-balance-inspector';

jest.setTimeout(300000); // 5 minutes

describe.each([originateFixedPriceTezOffchainSale])
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
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(tezos.bob);
    marketplace = await originateMarketplace(tezos.bob, bobAddress);
    marketAddress = marketplace.address;
    marketplaceAlice = await tezos.alice.contract.at(marketAddress);
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
  });
});
