import { bootstrap, TestTz } from './bootstrap-sandbox';
import { $log } from '@tsed/logger';
import { originateFtLimitedGlobalOperators, mintLimitedFtTokens, MintLimitedFtParam } from '../src/nft-contracts';
import { Contract, address, nat } from '../src/type-aliases';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';
import { char2Bytes } from '@taquito/tzip16';
import { QueryBalances, queryBalancesWithLambdaView, getBalances } from './fa2-balance-inspector';
import { getTokenMetadata, getTokenTotalSupply, transfer } from '../src/fa2-interface';

jest.setTimeout(180000); // 3 minutes

describe('Limited Fungible Token Contract', () => {
  let tezos: TestTz;
  let bobAddress : address;
  let aliceAddress : address;
  let ftLimitedBob : Contract;
  let queryBalances : QueryBalances;
  let token_info_bob : MichelsonMap< string, string>;
  let token_info_alice : MichelsonMap< string, string>;
  let ftLimitedAddress : address;
  let token0 : nat;
  let token1 : nat;
  let token0FixedSupply : nat;
  let token1FixedSupply : nat;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    ftLimitedBob = await originateFtLimitedGlobalOperators(tezos.bob, bobAddress, aliceAddress);
    ftLimitedAddress = ftLimitedBob.address;

    token0 = new BigNumber(0);
    token1 = new BigNumber(1);

    token0FixedSupply = new BigNumber(50);
    token1FixedSupply = new BigNumber(1);

    token_info_bob = new MichelsonMap();
    token_info_bob.set('name', char2Bytes('Bobs token'));
    token_info_bob.set('description', char2Bytes('description'));
    token_info_bob.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
    token_info_bob.set('symbol', char2Bytes('TK1'));

    token_info_alice = new MichelsonMap();
    token_info_alice.set('name', char2Bytes('Alices token'));
    token_info_alice.set('description', char2Bytes('description'));
    token_info_alice.set('ipfs_hash_image', char2Bytes('ipfs_hash_image'));
    token_info_alice.set('symbol', char2Bytes('TK1'));

  });

  test('mint limited ft', async () => {
    $log.info("Mint limited fts");
    const tokensBob : MintLimitedFtParam = {
      owner : bobAddress,
      amount : token0FixedSupply,
      token_info : token_info_bob,
    };
    const tokensAlice : MintLimitedFtParam = {
      owner : aliceAddress,
      amount : token1FixedSupply,
      token_info : token_info_alice,
    };
    await mintLimitedFtTokens(ftLimitedBob, [tokensBob, tokensAlice]);
    const [bobBalanceToken0, aliceBalanceToken0,
      bobBalanceToken1, aliceBalanceToken1] = await getBalances([
      { owner: bobAddress, token_id: token0 },
      { owner: aliceAddress, token_id: token0 },
      { owner: bobAddress, token_id: token1 },
      { owner: aliceAddress, token_id: token1 },
    ], queryBalances, ftLimitedBob);

    //TEST BALANCES
    expect(bobBalanceToken0.isEqualTo(token0FixedSupply)).toBe(true);
    expect(aliceBalanceToken0.isEqualTo(0)).toBe(true);
    expect(bobBalanceToken1.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken1.isEqualTo(token1FixedSupply)).toBe(true);

    //TEST TOKEN TOTAL SUPPLY
    const token0TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token0);
    const token1TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token1);
    expect(token0TotalSupply).toStrictEqual(token0FixedSupply);
    expect(token1TotalSupply).toStrictEqual(token1FixedSupply);

    //TEST TOKEN METADATA BOB
    const bobTokenMetadata : any = await getTokenMetadata(ftLimitedAddress, tezos.bob, token0);

    expect(bobTokenMetadata.token_id).toStrictEqual(token0);

    const entriesIteratorBob = bobTokenMetadata.token_info.entries();

    const descriptionIterateeBob = entriesIteratorBob.next();
    expect(descriptionIterateeBob.value[0]).toMatch('description');
    expect(descriptionIterateeBob.value[1]).toMatch(char2Bytes('description'));
    expect(descriptionIterateeBob.done).toBe(false);

    const ipfsIterateeBob = entriesIteratorBob.next();
    expect(ipfsIterateeBob.value[0]).toMatch('ipfs_hash_image');
    expect(ipfsIterateeBob.value[1]).toMatch(char2Bytes('ipfs_hash_image'));
    expect(ipfsIterateeBob.done).toBe(false);

    const nameIterateeBob = entriesIteratorBob.next();
    expect(nameIterateeBob.value[0]).toMatch('name');
    expect(nameIterateeBob.value[1]).toMatch(char2Bytes('Bobs token'));
    expect(nameIterateeBob.done).toBe(false);

    const symbolIterateeBob = entriesIteratorBob.next();
    expect(symbolIterateeBob.value[0]).toMatch('symbol');
    expect(symbolIterateeBob.value[1]).toMatch(char2Bytes('TK1'));
    expect(symbolIterateeBob.done).toBe(false);

    expect(entriesIteratorBob.next().done).toBe(true);

    //TEST TOKEN METADATA ALICE

    const aliceTokenMetadata : any = await getTokenMetadata(ftLimitedAddress, tezos.bob, token1);
    expect(aliceTokenMetadata.token_id).toStrictEqual(token1);

    const entriesIteratorAlice = aliceTokenMetadata.token_info.entries();

    const descriptionIterateeAlice = entriesIteratorAlice.next();
    expect(descriptionIterateeAlice.value[0]).toMatch('description');
    expect(descriptionIterateeAlice.value[1]).toMatch(char2Bytes('description'));
    expect(descriptionIterateeAlice.done).toBe(false);

    const ipfsIterateeAlice = entriesIteratorAlice.next();
    expect(ipfsIterateeAlice.value[0]).toMatch('ipfs_hash_image');
    expect(ipfsIterateeAlice.value[1]).toMatch(char2Bytes('ipfs_hash_image'));
    expect(ipfsIterateeAlice.done).toBe(false);

    const nameIterateeAlice = entriesIteratorAlice.next();
    expect(nameIterateeAlice.value[0]).toMatch('name');
    expect(nameIterateeAlice.value[1]).toMatch(char2Bytes('Alices token'));
    expect(nameIterateeAlice.done).toBe(false);

    const symbolIterateeAlice = entriesIteratorAlice.next();
    expect(symbolIterateeAlice.value[0]).toMatch('symbol');
    expect(symbolIterateeAlice.value[1]).toMatch(char2Bytes('TK1'));
    expect(symbolIterateeAlice.done).toBe(false);

    expect(entriesIteratorAlice.next().done).toBe(true);

  });
  test('global operator transfer succeeds', async () => {
    $log.info("Mint limited fts");
    const tokensBob : MintLimitedFtParam = {
      owner : bobAddress,
      amount : token0FixedSupply,
      token_info : token_info_bob,
    };
    const tokensAlice : MintLimitedFtParam = {
      owner : aliceAddress,
      amount : token1FixedSupply,
      token_info : token_info_alice,
    };
    await mintLimitedFtTokens(ftLimitedBob, [tokensBob, tokensAlice]);

    await transfer(ftLimitedAddress, tezos.alice, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: token0, amount: token0FixedSupply }],
        },
      ]);
    
    const [bobBalanceToken0, aliceBalanceToken0,
      bobBalanceToken1, aliceBalanceToken1] = await getBalances([
      { owner: bobAddress, token_id: token0},
      { owner: aliceAddress, token_id: token0 },
      { owner: bobAddress, token_id: token1},
      { owner: aliceAddress, token_id: token1 },
    ], queryBalances, ftLimitedBob);
    
    //TEST BALANCES
    expect(bobBalanceToken0.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken0.isEqualTo(token0FixedSupply)).toBe(true);
    expect(bobBalanceToken1.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken1.isEqualTo(token1FixedSupply)).toBe(true);

    //TEST TOKEN TOTAL SUPPLY
    const token0TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token0);
    const token1TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token1);
    expect(token0TotalSupply).toStrictEqual(token0FixedSupply);
    expect(token1TotalSupply).toStrictEqual(token1FixedSupply);

  });

  test('global operator transfer fails after removing global operator', async () => {
    $log.info("Mint limited fts");
    const tokensBob : MintLimitedFtParam = {
      owner : bobAddress,
      amount : token0FixedSupply,
      token_info : token_info_bob,
    };
    const tokensAlice : MintLimitedFtParam = {
      owner : aliceAddress,
      amount : token1FixedSupply,
      token_info : token_info_alice,
    };
    await mintLimitedFtTokens(ftLimitedBob, [tokensBob, tokensAlice]);
   
    const op = await ftLimitedBob.methods
      .update_global_operators([])
      .send();
    await op.confirmation(3);

    const transferOp = transfer(ftLimitedAddress, tezos.alice, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: token0, amount: token0FixedSupply }],
        },
      ]);

    return expect(transferOp).rejects.toHaveProperty('message', 'FA2_INSUFFICIENT_BALANCE');
    
  });

  test('global operator transfer succeeds after removing and readding global operator', async () => {
    $log.info("Mint limited fts");
    const tokensBob : MintLimitedFtParam = {
      owner : bobAddress,
      amount : token0FixedSupply,
      token_info : token_info_bob,
    };
    const tokensAlice : MintLimitedFtParam = {
      owner : aliceAddress,
      amount : token1FixedSupply,
      token_info : token_info_alice,
    };
    await mintLimitedFtTokens(ftLimitedBob, [tokensBob, tokensAlice]);
   
    const op1 = await ftLimitedBob.methods
      .update_global_operators([])
      .send();
    await op1.confirmation(3);

    const op2 = await ftLimitedBob.methods
      .update_global_operators([aliceAddress])
      .send();
    await op2.confirmation(3);

    await transfer(ftLimitedAddress, tezos.alice, [
        {
          from_: bobAddress,
          txs: [{ to_: aliceAddress, token_id: token0, amount: token0FixedSupply }],
        },
      ]);

    const [bobBalanceToken0, aliceBalanceToken0,
      bobBalanceToken1, aliceBalanceToken1] = await getBalances([
      { owner: bobAddress, token_id: token0},
      { owner: aliceAddress, token_id: token0 },
      { owner: bobAddress, token_id: token1},
      { owner: aliceAddress, token_id: token1 },
    ], queryBalances, ftLimitedBob);
    
    //TEST BALANCES
    expect(bobBalanceToken0.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken0.isEqualTo(token0FixedSupply)).toBe(true);
    expect(bobBalanceToken1.isEqualTo(0)).toBe(true);
    expect(aliceBalanceToken1.isEqualTo(token1FixedSupply)).toBe(true);

    //TEST TOKEN TOTAL SUPPLY
    const token0TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token0);
    const token1TotalSupply = await getTokenTotalSupply(ftLimitedAddress, tezos.bob, token1);
    expect(token0TotalSupply).toStrictEqual(token0FixedSupply);
    expect(token1TotalSupply).toStrictEqual(token1FixedSupply);
    
  });

  test('global operator update is admin checked', async () => {
    
    const ftLimitedAlice = await tezos.alice.contract.at(ftLimitedBob.address);
    const op = ftLimitedAlice.methods
      .update_global_operators([])
      .send();
    expect(op).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });
});
