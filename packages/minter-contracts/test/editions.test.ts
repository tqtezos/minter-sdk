import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import {
    BigMapAbstraction,
    TezosToolkit,
    MichelsonMap
} from '@taquito/taquito';


import { char2Bytes } from '@taquito/tzip16';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, nat, bytes, address} from '../src/type-aliases';

import {
    originateEditionsNftContract,
    MintNftParam
} from '../src/nft-contracts';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens} from './fa2-balance-inspector';

jest.setTimeout(180000); // 3 minutes


export interface MintEditionParam {
  edition_info : MichelsonMap<string, string>;
  number_of_editions : nat;
}

export interface distribute_edition {
  edition_id : nat; 
  receivers : address[];
} 

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftEditions : Contract;
  let nft1 : MintEditionParam;
  let nft2 : MintEditionParam;
  let tokenId : nat;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let bobAddress : address;
  let aliceAddress : address;
  let queryBalances: QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    tokenId = new BigNumber(0);
    empty_metadata_map = new MichelsonMap();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    $log.info('originating editions contract')
    nftEditions = await originateEditionsNftContract(tezos.bob, bobAddress);
    $log.info('editions contract originated')
  });
  
  test('mint 1000 editions of nft1 and 2 of nft2', async() => {
    nft1 = {
      edition_info: empty_metadata_map,
      number_of_editions : new BigNumber(1000)
    };

    nft2 = {
      edition_info: empty_metadata_map,
      number_of_editions : new BigNumber(2)
    };
    const opMint = await nftEditions.methods.mint_editions([nft1, nft2]).send();
    const hash = await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`) 
  });
  
  test('distribute editions', async() => {
    const distributeEdition0 : distribute_edition = {
      edition_id : new BigNumber(0),
      receivers : [aliceAddress, bobAddress]
    }
    const distributeEdition1 : distribute_edition = {
      edition_id : new BigNumber(1),
      receivers : [aliceAddress, bobAddress]
    }
    const opDistribute = await nftEditions.methods.distribute_editions([distributeEdition0, distributeEdition1]).send();
    const hash = await opDistribute.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opDistribute.consumedGas}`) 

    const [aliceHasEdition0, bobHasEdition0] = await hasTokens([
      { owner: aliceAddress, token_id: new BigNumber(0) },
      {owner: bobAddress, token_id: new BigNumber(1)}
      ], queryBalances, nftEditions);

    const [aliceHasEdition1, bobHasEdition1] = await hasTokens([
      { owner: aliceAddress, token_id: new BigNumber(1000) },
      {owner: bobAddress, token_id: new BigNumber(1001)}
      ], queryBalances, nftEditions);
  
    expect(aliceHasEdition0).toBe(true);
    expect(bobHasEdition0).toBe(true);
    expect(aliceHasEdition1).toBe(true);
    expect(bobHasEdition1).toBe(true);
  });
  test('distributing too many editions should fail', async() => {
    const distributeEdition1 : distribute_edition = {
      edition_id : new BigNumber(1),
      receivers : [aliceAddress]
    }
    const opDistribute = nftEditions.methods.distribute_editions([distributeEdition1]).send();
    return expect(opDistribute).rejects.toHaveProperty('errors');
  });
});
