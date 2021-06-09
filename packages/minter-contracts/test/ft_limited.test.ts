import { bootstrap, TestTz } from './bootstrap-sandbox';
import { $log } from '@tsed/logger';
import { originateFtLimited, mintLimitedFtTokens, MintLimitedFtParam } from '../src/nft-contracts';
import { Contract, address } from '../src/type-aliases';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';
import { char2Bytes } from '@taquito/tzip16';
import { QueryBalances, queryBalancesWithLambdaView, getBalances } from './fa2-balance-inspector';

jest.setTimeout(180000); // 3 minutes

describe('Limited Fungible Token Contract', () => {
  let tezos: TestTz;
  let bobAddress : address;
  let aliceAddress : address;
  let ftLimitedBob : Contract;
  let queryBalances: QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    ftLimitedBob = await originateFtLimited(tezos.bob, bobAddress);
  });

  test('mint limited ft', async () => {
    $log.info("Mint limited fts");
    const token_info: MichelsonMap< string, string> = new MichelsonMap();
    token_info.set('name', char2Bytes('A token'));
    token_info.set('description', char2Bytes('description'));
    token_info.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
    token_info.set('symbol', char2Bytes('TK1'));
    const tokensBob : MintLimitedFtParam = {
      owner : bobAddress,
      amount : new BigNumber(50),
      token_info : token_info,
    };
    const tokensAlice : MintLimitedFtParam = {
      owner : aliceAddress,
      amount : new BigNumber(1),
      token_info : token_info,
    };
    await mintLimitedFtTokens(ftLimitedBob, [tokensBob, tokensAlice]);
    const [bobBalanceToken0, aliceBalanceToken0,
      bobBalanceToken1, aliceBalanceToken1] = await getBalances([
      { owner: bobAddress, token_id: new BigNumber(0) },
      { owner: aliceAddress, token_id: new BigNumber(0) },
      { owner: bobAddress, token_id: new BigNumber(1) },
      { owner: aliceAddress, token_id: new BigNumber(1) },
    ], queryBalances, ftLimitedBob);

    expect(bobBalanceToken0.isEqualTo(50)).toBe(true);
    expect(aliceBalanceToken0.isEqualTo(0)).toBe(true);
    expect(bobBalanceToken1.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken1.isEqualTo(1)).toBe(true);
  });

});
