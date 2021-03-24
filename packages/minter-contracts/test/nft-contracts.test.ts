import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import {
  BigMapAbstraction,
  TezosToolkit,
  MichelsonMap,
} from '@taquito/taquito';


import { char2Bytes } from '@taquito/tzip16';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, nat } from '../src/type-aliases';
// import { assertMichelsonType, BytesLiteral } from '@taquito/michel-codec';

import {
  originateNftFaucet,
  originateNft,
  MintNftParam,
} from '../src/nft-contracts';
import {
  BalanceOfRequest,
  transfer,
  addOperator,
  removeOperator,
} from '../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens } from './fa2-balance-inspector';

jest.setTimeout(180000); // 3 minutes

const nat1 = new BigNumber(1);

describe.each([originateNftFaucet])(
  'test NFT',
  createNft => {
    let tezos: TestTz;
    let nft: Contract;
    let queryBalances: QueryBalances;

    beforeAll(async () => {
      tezos = await bootstrap();
      queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    });

    beforeEach(async () => {
      const admin = await tezos.bob.signer.publicKeyHash();
      nft = await createNft(tezos.bob, admin);
    });

    async function mintTokens(
      tz: TezosToolkit,
      tokens: MintNftParam[],
    ): Promise<void> {
      $log.info('minting...');
      const op = await nft.methods.mint(tokens).send();
      const hash = await op.confirmation();
      $log.info(`minted tokens. consumed gas: ${op.consumedGas}`);
    }

    test('mint token', async () => {
      const bobAddress = await tezos.bob.signer.publicKeyHash();
      const token_info: MichelsonMap<
                string,
                string
            > = new MichelsonMap();

      token_info.set('name', char2Bytes('A token'));
      token_info.set('description', char2Bytes('description'));
      token_info.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
      token_info.set('symbol', char2Bytes('TK1'));

      await mintTokens(tezos.bob, [
        {
          token_metadata: {
            token_id: new BigNumber(0),
            token_info,
          },
          owner: bobAddress,
        },
      ]);

      const [bobHasToken] = await hasTokens([
        { owner: bobAddress, token_id: new BigNumber(0) },
      ], queryBalances, nft);
      expect(bobHasToken).toBe(true);

      const storage: any = await nft.storage();
      const assets = await storage.assets;

      const ret = await assets.token_metadata.get('0');

      expect(ret.token_id).toStrictEqual(new BigNumber(0));

      const entriesIterator = ret.token_info.entries();

      const descriptionIteratee = entriesIterator.next();
      expect(descriptionIteratee.value[0]).toMatch('description');
      expect(descriptionIteratee.value[1]).toMatch(char2Bytes('description'));
      expect(descriptionIteratee.done).toBe(false);

      const ipfsIteratee = entriesIterator.next();
      expect(ipfsIteratee.value[0]).toMatch('ipfs_hash_image');
      expect(ipfsIteratee.value[1]).toMatch(char2Bytes('ipfs_hash_image'));
      expect(ipfsIteratee.done).toBe(false);

      const nameIteratee = entriesIterator.next();
      expect(nameIteratee.value[0]).toMatch('name');
      expect(nameIteratee.value[1]).toMatch(char2Bytes('A token'));
      expect(nameIteratee.done).toBe(false);

      const symbolIteratee = entriesIterator.next();
      expect(symbolIteratee.value[0]).toMatch('symbol');
      expect(symbolIteratee.value[1]).toMatch(char2Bytes('TK1'));
      expect(symbolIteratee.done).toBe(false);

      expect(entriesIterator.next().done).toBe(true);
    });

    test('transfer token', async () => {
      const aliceAddress = await tezos.alice.signer.publicKeyHash();
      const bobAddress = await tezos.bob.signer.publicKeyHash();
      const tokenId = new BigNumber(0);
      const token_info: MichelsonMap<
                string,
                string
            > = new MichelsonMap();

      token_info.set('name', char2Bytes('A token'));
      token_info.set('description', char2Bytes('description'));
      token_info.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
      token_info.set('symbol', char2Bytes('TK1'));

      await mintTokens(tezos.bob, [
        {
          token_metadata: {
            token_id: tokenId,
            token_info,
          },
          owner: bobAddress,
        },
      ]);

      const [aliceHasATokenBefore, bobHasATokenBefore] = await hasTokens([
        { owner: aliceAddress, token_id: tokenId },
        { owner: bobAddress, token_id: tokenId },
      ], queryBalances, nft);
      expect(aliceHasATokenBefore).toBe(false);
      expect(bobHasATokenBefore).toBe(true);

      await transfer(nft.address, tezos.bob, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: tokenId, amount: nat1 }],
        },
      ]);

      const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
        { owner: aliceAddress, token_id: tokenId },
        { owner: bobAddress, token_id: tokenId },
      ], queryBalances, nft);
      expect(aliceHasATokenAfter).toBe(true);
      expect(bobHasATokenAfter).toBe(false);
    });

    test('transfer not by owner must fail', async () => {
      const aliceAddress = await tezos.alice.signer.publicKeyHash();
      const bobAddress = await tezos.bob.signer.publicKeyHash();
      const tokenId = new BigNumber(0);
      const token_info: MichelsonMap<
                string,
                string
            > = new MichelsonMap();

      token_info.set('name', char2Bytes('A token'));
      token_info.set('description', char2Bytes('description'));
      token_info.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
      token_info.set('symbol', char2Bytes('TK1'));

      await mintTokens(tezos.bob, [
        {
          owner: bobAddress,
          token_metadata: {
            token_id: tokenId,
            token_info,
          },
        },
      ]);

      // alice is trying to transfer tokens on behalf of bob
      const p = transfer(nft.address, tezos.alice, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: tokenId, amount: nat1 }],
        },
      ]);

      await expect(p).rejects.toHaveProperty('message', 'FA2_NOT_OPERATOR');
    });

    test('transfer by operator', async () => {
      const aliceAddress = await tezos.alice.signer.publicKeyHash();
      const bobAddress = await tezos.bob.signer.publicKeyHash();
      const tokenId1 = new BigNumber(0);
      const tokenId2 = new BigNumber(1);
      const token_info_bob: MichelsonMap<
                string,
                string
            > = new MichelsonMap();

      token_info_bob.set('name', char2Bytes('A token'));
      token_info_bob.set('description', char2Bytes('description'));
      token_info_bob.set(
        'ipfs_hash_image',
        char2Bytes('ipfs_hash_image'),
      );
      token_info_bob.set('symbol', char2Bytes('TK1'));

      const token_info_alice: MichelsonMap<
                string,
                string
            > = new MichelsonMap();

      token_info_alice.set('name', char2Bytes('B token'));
      token_info_alice.set('description', char2Bytes('description'));
      token_info_alice.set(
        'ipfs_hash_image',
        char2Bytes('ipfs_hash_image'),
      );
      token_info_alice.set('symbol', char2Bytes('TK2'));

      await mintTokens(tezos.bob, [
        {
          owner: bobAddress,
          token_metadata: {
            token_id: tokenId1,
            token_info: token_info_bob,
          },
        },
        {
          owner: aliceAddress,
          token_metadata: {
            token_id: tokenId2,
            token_info: token_info_alice,
          },
        },
      ]);

      // check initial balances
      const [
        aliceHasToken1Before,
        aliceHasToken2Before,
        bobHasToken1Before,
        bobHasToken2Before,
      ] = await hasTokens([
        { owner: aliceAddress, token_id: tokenId1 },
        { owner: aliceAddress, token_id: tokenId2 },
        { owner: bobAddress, token_id: tokenId1 },
        { owner: bobAddress, token_id: tokenId2 },
      ], queryBalances, nft);
      expect(aliceHasToken1Before).toBe(false);
      expect(aliceHasToken2Before).toBe(true);
      expect(bobHasToken1Before).toBe(true);
      expect(bobHasToken2Before).toBe(false);

      await addOperator(nft.address, tezos.alice, bobAddress, tokenId2);

      // swap tokens
      await transfer(nft.address, tezos.bob, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: tokenId1, amount: nat1 }],
        },
        {
          from_: aliceAddress,
          txs: [{ to_: bobAddress, token_id: tokenId2, amount: nat1 }],
        },
      ]);

      await removeOperator(nft.address, tezos.alice, bobAddress, tokenId2);

      // check balances after the swap
      const [
        aliceHasToken1After,
        aliceHasToken2After,
        bobHasToken1After,
        bobHasToken2After,
      ] = await hasTokens([
        { owner: aliceAddress, token_id: tokenId1 },
        { owner: aliceAddress, token_id: tokenId2 },
        { owner: bobAddress, token_id: tokenId1 },
        { owner: bobAddress, token_id: tokenId2 },
      ], queryBalances, nft);
      expect(aliceHasToken1After).toBe(true);
      expect(aliceHasToken2After).toBe(false);
      expect(bobHasToken1After).toBe(false);
      expect(bobHasToken2After).toBe(true);
    });
  },
);
