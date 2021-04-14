import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import {
  Context,
  Extension,
  MichelsonMap,
} from '@taquito/taquito';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, nat, bytes, address } from '../src/type-aliases';

import {
  originateEditionsNftContract,
} from '../src/nft-contracts';
import {
  transfer,
} from '../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens } from './fa2-balance-inspector';
import { Tzip16Module, tzip16 } from '@taquito/tzip16';
import { Fa2MultiNftTokenEditionsContractType } from '..';

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
  let nftEditions : Contract<Fa2MultiNftTokenEditionsContractType>;
  let nft1 : MintEditionParam;
  let nft2 : MintEditionParam;
  let edition_1_metadata : MichelsonMap<string, bytes>;
  let edition_2_metadata : MichelsonMap<string, bytes>;
  let bobAddress : address;
  let aliceAddress : address;
  let queryBalances: QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    edition_1_metadata = new MichelsonMap();
    edition_1_metadata.setType({ prim :"map", args :[{ prim:"string" }, { prim:"bytes" }] });
    edition_1_metadata.set("name", "66616b65206e616d65");
    edition_2_metadata = new MichelsonMap();
    edition_2_metadata.setType({ prim :"map", args :[{ prim:"string" }, { prim:"bytes" }] });
    edition_2_metadata.set("name", "74657374206e616d65");
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    $log.info('originating editions contract');
    nftEditions = await originateEditionsNftContract(tezos.bob, bobAddress);
    $log.info(`editions contract originated`);
  });

  test('mint 1000 editions of nft1 and 2 of nft2', async() => {
    nft1 = {
      edition_info : edition_1_metadata,
      number_of_editions : new BigNumber(1000),
    };

    nft2 = {
      edition_info: edition_2_metadata,
      number_of_editions : new BigNumber(2),
    };
    const opMint = await nftEditions.methods.mint_editions([nft1, nft2]).send();
    await opMint.confirmation();
    $log.info(`Minted editions. Consumed gas: ${opMint.consumedGas}`);
  });

  test('distribute editions', async() => {
    const distributeEdition0 : distribute_edition = {
      edition_id : new BigNumber(0),
      receivers : [aliceAddress, bobAddress],
    };
    const distributeEdition1 : distribute_edition = {
      edition_id : new BigNumber(1),
      receivers : [aliceAddress, bobAddress],
    };
    const opDistribute = await nftEditions.methods.distribute_editions([distributeEdition0, distributeEdition1]).send();
    await opDistribute.confirmation();
    $log.info(`Distributed editions. Consumed gas: ${opDistribute.consumedGas}`);

    const [aliceHasEdition0, bobHasEdition0] = await hasTokens([
      { owner: aliceAddress, token_id: new BigNumber(0) },
      { owner: bobAddress, token_id: new BigNumber(1) },
    ], queryBalances, nftEditions);

    const [aliceHasEdition1, bobHasEdition1] = await hasTokens([
      { owner: aliceAddress, token_id: new BigNumber(1000) },
      { owner: bobAddress, token_id: new BigNumber(1001) },
    ], queryBalances, nftEditions);

    expect(aliceHasEdition0).toBe(true);
    expect(bobHasEdition0).toBe(true);
    expect(aliceHasEdition1).toBe(true);
    expect(bobHasEdition1).toBe(true);
  });
  test('distributing too many editions should fail', async() => {
    const distributeEdition1 : distribute_edition = {
      edition_id : new BigNumber(1),
      receivers : [aliceAddress],
    };
    const opDistribute = nftEditions.methods.distribute_editions([distributeEdition1]).send();
    return expect(opDistribute).rejects.toHaveProperty('errors');
  });

  test ('transfer edition', async() => {
    const tokenId = new BigNumber(0);
    const nat1 = new BigNumber(1);
    await transfer(nftEditions.address, tezos.alice, [
      {
        from_: aliceAddress,
        txs: [{ to_: bobAddress, token_id: tokenId, amount: nat1 }],
      },
    ]);

    const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
      { owner: bobAddress, token_id: tokenId },
    ], queryBalances, nftEditions);
    expect(aliceHasATokenAfter).toBe(false);
    expect(bobHasATokenAfter).toBe(true);
  });

  test ('test editions token-metadata with off-chain view', async() => {
    tezos.bob.addExtension(new Tzip16Module());
    const editionsContractMetadata = await tezos.bob.contract.at(nftEditions.address, tzip16);
    $log.info(`Initialising the views for editions contract ...`);
    const views = await editionsContractMetadata.tzip16().metadataViews();
    $log.info(`The following view names were found in the metadata: ${Object.keys(views)}`);
    $log.info(`get metadata for edition with token_id 0 ...`);
    const token0Metadata = await views.token_metadata().executeView(0);
    expect(token0Metadata).toEqual({
      token_id : new BigNumber(0),
      token_info : edition_1_metadata,
    });
  });
});
