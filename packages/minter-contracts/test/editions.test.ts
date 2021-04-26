import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import {
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

jest.setTimeout(180000); // 3 minutes


export interface MintEditionParam {
  edition_info: MichelsonMap<string, string>;
  number_of_editions: nat;
}

export interface distribute_edition {
  edition_id: nat;
  receivers: address[];
}

describe('test NFT auction', () => {
  let maxEditions: nat;
  let tezos: TestTz;
  let nftEditionsBob: Contract;
  let nftEditionsAlice: Contract;
  let nft1: MintEditionParam;
  let nft2: MintEditionParam;
  let edition_1_metadata: MichelsonMap<string, bytes>;
  let edition_2_metadata: MichelsonMap<string, bytes>;
  let bobAddress: address;
  let aliceAddress: address;
  let queryBalances: QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    edition_1_metadata = new MichelsonMap();
    edition_1_metadata.setType({ prim: "map", args: [{ prim: "string" }, { prim: "bytes" }] });
    edition_1_metadata.set("name", "66616b65206e616d65");
    edition_2_metadata = new MichelsonMap();
    edition_2_metadata.setType({ prim: "map", args: [{ prim: "string" }, { prim: "bytes" }] });
    edition_2_metadata.set("name", "74657374206e616d65");
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    $log.info('originating editions contract');
    nftEditionsBob = await originateEditionsNftContract(tezos.bob, bobAddress);
    nftEditionsAlice = await tezos.alice.contract.at(nftEditionsBob.address);
    $log.info(`editions contract originated`);
    const contractStorage : any = await nftEditionsBob.storage();
    maxEditions = await contractStorage.max_editions_per_run;
  });

  test('change admin by non admin should fail', async () => {
    const opSetAdmin = nftEditionsAlice.methods.set_admin(aliceAddress).send();
    return expect(opSetAdmin).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

  test('pause by non admin should fail', async () => {
    const opPause = nftEditionsAlice.methods.pause([true]).send();
    return expect(opPause).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

  test('change admin by admin should succeed', async () => {
    $log.info("Testing change admin");
    const opSetAdmin = await nftEditionsBob.methods.set_admin(aliceAddress).send();
    await opSetAdmin.confirmation();
    const opConfirmAdmin = await nftEditionsAlice.methods.confirm_admin(["unit"]).send();
    await opConfirmAdmin.confirmation();
    const contractStorage1 : any = await nftEditionsBob.storage();
    const admin = await contractStorage1.nft_asset_storage.admin.admin;
    expect(admin).toEqual(aliceAddress);
    $log.info("Admin changed successfully");

    $log.info("Set admin back");
    const opSetAdminBack = await nftEditionsAlice.methods.set_admin(bobAddress).send();
    await opSetAdminBack.confirmation();
    const opConfirmAdminBob = await nftEditionsBob.methods.confirm_admin(["unit"]).send();
    await opConfirmAdminBob.confirmation();
    const contractStorage2 : any = await nftEditionsBob.storage();
    const finalAdmin = await contractStorage2.nft_asset_storage.admin.admin;
    expect(finalAdmin).toEqual(bobAddress);
    $log.info("Admin changed back successfully");
  });

  test('minting by non admin should fail', async () => {
    const nft = {
      edition_info: edition_1_metadata,
      number_of_editions: new BigNumber(1000),
    };
    const opMint = nftEditionsAlice.methods.mint_editions([nft]).send();
    return expect(opMint).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

  test('minting too large of an edition set should fail', async () => {
    const nft = {
      edition_info: edition_1_metadata,
      number_of_editions: maxEditions.plus(1),
    };
    const opMint = nftEditionsBob.methods.mint_editions([nft]).send();
    return expect(opMint).rejects.toHaveProperty('message', 'EDITION_RUN_TOO_LARGE');
  });


  test('mint 1000 editions of nft1 and 2 of nft2', async () => {
    nft1 = {
      edition_info: edition_1_metadata,
      number_of_editions: new BigNumber(1000),
    };

    nft2 = {
      edition_info: edition_2_metadata,
      number_of_editions: new BigNumber(2),
    };
    const opMint = await nftEditionsBob.methods.mint_editions([nft1, nft2]).send();
    await opMint.confirmation();
    $log.info(`Minted editions. Consumed gas: ${opMint.consumedGas}`);
  });

  test('distribute editions', async () => {
    const distributeEdition0: distribute_edition = {
      edition_id: new BigNumber(0),
      receivers: [aliceAddress, bobAddress],
    };
    const distributeEdition1: distribute_edition = {
      edition_id: new BigNumber(1),
      receivers: [aliceAddress, bobAddress],
    };
    const opDistribute = await nftEditionsBob.methods
      .distribute_editions([distributeEdition0, distributeEdition1]).send();
    await opDistribute.confirmation();
    $log.info(`Distributed editions. Consumed gas: ${opDistribute.consumedGas}`);

    const [aliceHasEdition0, bobHasEdition0] = await hasTokens([
      { owner: aliceAddress, token_id: new BigNumber(0) },
      { owner: bobAddress, token_id: new BigNumber(1) },
    ], queryBalances, nftEditionsBob);

    const [aliceHasEdition1, bobHasEdition1] = await hasTokens([
      { owner: aliceAddress, token_id: maxEditions },
      { owner: bobAddress, token_id: maxEditions.plus(1) },
    ], queryBalances, nftEditionsBob);

    expect(aliceHasEdition0).toBe(true);
    expect(bobHasEdition0).toBe(true);
    expect(aliceHasEdition1).toBe(true);
    expect(bobHasEdition1).toBe(true);
  });
  test('distributing too many editions should fail', async () => {
    const distributeEdition1: distribute_edition = {
      edition_id: new BigNumber(1),
      receivers: [aliceAddress],
    };
    const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition1]).send();
    return expect(opDistribute).rejects.toHaveProperty('message', 'NO_EDITIONS_TO_DISTRIBUTE');
  });

  test('transfer edition', async () => {
    const tokenId = new BigNumber(0);
    const nat1 = new BigNumber(1);
    await transfer(nftEditionsBob.address, tezos.alice, [
      {
        from_: aliceAddress,
        txs: [{ to_: bobAddress, token_id: tokenId, amount: nat1 }],
      },
    ]);
    const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
      { owner: bobAddress, token_id: tokenId },
    ], queryBalances, nftEditionsBob);
    expect(aliceHasATokenAfter).toBe(false);
    expect(bobHasATokenAfter).toBe(true);
  });

  test('test editions token-metadata with off-chain view', async () => {
    tezos.bob.addExtension(new Tzip16Module());
    const editionsContractMetadata = await tezos.bob.contract.at(nftEditionsBob.address, tzip16);
    $log.info(`Initialising the views for editions contract ...`);
    const views = await editionsContractMetadata.tzip16().metadataViews();
    $log.info(`The following view names were found in the metadata: ${Object.keys(views)}`);
    $log.info(`get metadata for edition with token_id 0 ...`);
    const token0Metadata = await views.token_metadata().executeView(0);
    expect(token0Metadata).toEqual({
      token_id: new BigNumber(0),
      token_info: edition_1_metadata,
    });
  });

  test('Distributing from an edition set you did not create should fail', async () => {
    const distributeEdition0: distribute_edition = {
      edition_id: new BigNumber(0),
      receivers: [aliceAddress],
    };
    const opDistribute = nftEditionsAlice.methods.distribute_editions([distributeEdition0]).send();
    return expect(opDistribute).rejects.toHaveProperty('message', 'INVALID_DISTRIBUTOR');
  });

  test('Distributing from a non existing edition set should fail', async () => {
    const distributeEdition5: distribute_edition = {
      edition_id: new BigNumber(5),
      receivers: [aliceAddress],
    };
    const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition5]).send();
    return expect(opDistribute).rejects.toHaveProperty('message', 'INVALID_EDITION_ID');
  });

  test('Distributing while contract is paused should fail', async () => {
    $log.info("pausing the contract");
    const opPause = await nftEditionsBob.methods.pause([true]).send();
    await opPause.confirmation();
    $log.info("contract paused");
    const distributeEdition0: distribute_edition = {
      edition_id: new BigNumber(0),
      receivers: [aliceAddress],
    };
    const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition0]).send();
    return expect(opDistribute).rejects.toHaveProperty('message', 'PAUSED');
  });

});
