import { $log } from '@tsed/logger';
import {
  MichelsonMap,
} from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

import { bootstrapWithoutLambdaView, TestTz } from './bootstrap-sandbox';
import { Contract, bytes, address, nat } from '../src/type-aliases';

import { originateBondingCurve } from '../src/bonding-curve';
import { char2Bytes } from '@taquito/tzip16';
// import { originateNft } from '../src/nft-contracts';
import {
  addOperator,
  // transfer,
} from '../src/fa2-interface';
// import { QueryBalances, queryBalancesWithLambdaView, hasTokens } from './fa2-balance-inspector';

// because originateNft doesn't allow raw storage
// could fix by making originateNftRawStorage
import { originateContract } from '../src/ligo';
import { Fa2MultiNftAssetCode } from '../bin-ts';


jest.setTimeout(180000); // 3 minutes


export interface MintEditionParam {
  edition_info: MichelsonMap<string, string>;
  number_of_editions: nat;
}

export interface distribute_edition {
  edition_id: nat;
  receivers: address[];
}

// TODO
describe('bonding-curve: test NFT auction', () => {
  // let maxEditions: nat;
  // let nftEditionsBob: Contract;
  let tezos: TestTz;

  let bondingCurve: Contract;
  let nft: Contract;

  // let nftEditionsAlice: Contract;
  // let nft1: MintEditionParam;
  // let nft2: MintEditionParam;
  let edition_1_metadata: MichelsonMap<string, bytes>;
  let edition_2_metadata: MichelsonMap<string, bytes>;
  let adminAddress: address;
  let aliceAddress: address;
  let bobAddress: address;
  let charlieAddress: address;
  // let queryBalances: QueryBalances;

  beforeAll(async () => {
    // skip lambda view contract for now for speed
    // tezos = await bootstrap();
    tezos = await bootstrapWithoutLambdaView();
    edition_1_metadata = new MichelsonMap();
    edition_1_metadata.setType({ prim: "map", args: [{ prim: "string" }, { prim: "bytes" }] });
    edition_1_metadata.set("name", "66616b65206e616d65");
    edition_2_metadata = new MichelsonMap();
    edition_2_metadata.setType({ prim: "map", args: [{ prim: "string" }, { prim: "bytes" }] });
    edition_2_metadata.set("name", "74657374206e616d65");

    // eve is admin
    adminAddress = await tezos.eve.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    charlieAddress = await tezos.charlie.signer.publicKeyHash();

    // queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    //
    // $log.info('originating editions contract');
    // nftEditionsBob = await originateEditionsNftContract(tezos.bob, bobAddress);

    // const bondingCurveStorage: BondingCurveContractType["storage"] =
    // {
    //   admin: {
    //     admin: bobAddress as bin_address,
    //     paused: false,
    //     pending_admin: undefined,
    //   },

    //   // market_contract: nftEditionsBob.address as bin_address,
    //   market_contract: bobAddress as bin_address,

    //   auction_price: new BigNumber(0) as bin_mutez,
    //   token_index: new BigNumber(0) as bin_nat,
    //   token_metadata: {

    //   },

    //   // 1%
    //   basis_points: new BigNumber(100) as bin_nat,

    //   // Linear example:
    //   // Cost(x) = x = 0 * x^0 + 1 * x^1
    //   cost_mutez: {
    //     segments: [],
    //     last_segment: [new BigNumber(0) as bin_int, new BigNumber(1) as bin_int],
    //   },
    //   unclaimed: new BigNumber(0) as bin_mutez,
    // };


    // exampleTokenMetadata :: TokenMetadata
    // exampleTokenMetadata = mkTokenMetadata symbol name decimals
    //   where
    //     symbol = "test_symbol"
    //     name = "This is a test! [name]"
    //     decimals = "12"

    // exampleStorage' = Storage
    //   { admin = exampleAdminStorage
    //   , market_contract = detGenKeyAddress "dummy-impossible-contract-key"
    //   , auction_price = toMutez 0
    //   , token_index = 2
    //   , token_metadata = exampleTokenMetadata
    //   , basis_points = 100
    //   , cost_mutez = examplePiecewisePolynomial'
    //   , unclaimed = toMutez 3
    //   }

    // ("admin","Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None")
    // ("market_contract","\"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"")
    // storage for distinguishing fields:


    // const bondingCurveStorageString2 = `
    //   { Pair (Pair "${adminAddress}" False) None; "${market_contractAddress}"; ${auction_price}; ${token_index};
    //     {
    //       Elt "decimals" 0x3132;
    //       Elt "name" 0x546869732069732061207465737421205b6e616d655d;
    //       Elt "symbol" 0x746573745f73796d626f6c };
    //     ${basis_points}; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 0 }`;

    // expect(bondingCurveStorageString).toBe(bondingCurveStorageString2);

    // const bondingCurveStorageString = "{ Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None;
    // \"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"; 0; 0; { Elt \"decimals\" 0x3132;
    // Elt \"name\" 0x546869732069732061207465737421205b6e616d655d;
    // Elt \"symbol\" 0x746573745f73796d626f6c }; 100; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 0 }";

    // before storage update
    // const bondingCurveStorageString = `
    //   { Pair (Pair "${adminAddress}" False) None; "${market_contractAddress}"; ${auction_price}; ${token_index};
    //     Pair 42 {
    //       Elt "decimals" 0x3132;
    //       Elt "name" 0x546869732069732061207465737421205b6e616d655d;
    //       Elt "symbol" 0x746573745f73796d626f6c };
    //     ${basis_points}; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 0 }`;


    // nftEditionsAlice = await tezos.alice.contract.at(nftEditionsBob.address);
    // $log.info(`editions contract originated`);
    // const contractStorage : any = await nftEditionsBob.storage();
    // maxEditions = await contractStorage.max_editions_per_run;
  });

  // test('Minimal test to originate', async () => {
  //   $log.info("Minimal test to originate");

  //   const adminAddress = aliceAddress;
  //   const market_contractAddress = aliceAddress;
  //   const auction_price = 0;
  //   const token_index = 0;
  //   const basis_points = 100;

  //   const token_name = "test_symbol";
  //   const token_symbol = "This is a test! [name]";
  //   const token_decimals = "12";

  //   // examplePiecewisePolynomial' = PiecewisePolynomial
  //   //   { segments = [(6, [7, 8])]
  //   //   , last_segment = [4, 5]
  //   //   }
  //   const segments = '{ Pair 6 { 7; 8 } }';
  //   const last_segment = '{ 4; 5 }';
  //   const unclaimed_mutez = 0;

  //   const bondingCurveStorageString = `
  //     { Pair (Pair "${adminAddress}" False) None; "${market_contractAddress}"; ${auction_price}; ${token_index};
  //       {
  //         Elt "decimals" 0x${char2Bytes(token_decimals)};
  //         Elt "name" 0x${char2Bytes(token_name)};
  //         Elt "symbol" 0x${char2Bytes(token_symbol)} };
  //       ${basis_points}; Pair ${segments} ${last_segment}; ${unclaimed_mutez}
  //     }`;

  //   $log.info(`originating bonding curve contract with storage:\n${bondingCurveStorageString}`);
  //   // bondingCurve = await originateBondingCurve(tezos.bob, bondingCurveStorage as Record<string, any>);
  //   bondingCurve = await originateBondingCurve(tezos.bob, bondingCurveStorageString);
  //   $log.info(`bonding curve contract originated: ${bondingCurve}`);

  //   expect('ok').toBe('ok');
  // });


  test('Buy sell test', async () => {

    // (admin, alice, bob, charlie)
    // ("admin_address","alice_address","bob_address","charlie_address")

    const admin_address = adminAddress;
    const admin_toolkit = tezos.eve;

    // nft storage
    // const nft_storage =
    //   `Pair { Pair (Pair "${admin_address}" False) None; Pair { Elt 0 "${admin_address}" } 1; { }; { } } { }`;

    const meta_uri = char2Bytes('tezos-storage:content');
    const sample_metadata = {
      name: 'example_name',
      description: 'sample_token',
      interfaces: ['TZIP-012', 'TZIP-016'],
    };
    const meta_content = char2Bytes(JSON.stringify(sample_metadata, null, 2));

    const nft_storage = `(Pair (Pair (Pair (Pair "${admin_address}" False) None) (Pair (Pair { Elt 0 "${admin_address}" } 1) (Pair { } { }))) { Elt "" 0x${meta_uri} ; Elt "content" 0x${meta_content} })`;


    $log.info(`originating nft contract with storage:\n${nft_storage}`);
    const nft_contract = await originateContract(tezos.bob, Fa2MultiNftAssetCode.code, nft_storage, 'nft');
    const nft_address = nft_contract.address;

    const bonding_curve_storage =
      `{ Pair (Pair "${admin_address}" False) None; "${nft_address}"; 100; 0; { Elt "decimals" 0x3132; Elt "name" 0x546869732069732061207465737421205b6e616d655d; Elt "symbol" 0x746573745f73796d626f6c }; 100; Pair { } { 10; 20; 30 }; 0 }`;

    $log.info(`originating bonding curve contract with storage:\n${bonding_curve_storage}`);
    const bonding_curve_contract = await originateBondingCurve(tezos.bob, bonding_curve_storage);
    const bonding_curve_address = bonding_curve_contract.address;

    $log.info("admin -> nft: update_operators (bonding curve -> token_id=0)");
    const op_update_operators = await addOperator(nft_address, admin_toolkit, bonding_curve_address, new BigNumber(0));

    const bonding_curve_alice = await tezos.alice.contract.at(bonding_curve_contract.address);
    const bonding_curve_bob = await tezos.bob.contract.at(bonding_curve_contract.address);
    const bonding_curve_charlie = await tezos.charlie.contract.at(bonding_curve_contract.address);

    // alice -> bondingCurve: buy
    $log.info(`alice -> bondingCurve: buy`);
    const alice_buy_op = await bonding_curve_alice.methods.buy().send({ amount: 111, mutez: true });
    await alice_buy_op.confirmation();

    try {

      // bob -> bondingCurve: buy
      $log.info(`bob -> bondingCurve: buy`);
      const bob_buy_op = await bonding_curve_bob.methods.buy().send({ amount: 161, mutez: true });
      await bob_buy_op.confirmation();

    } catch (ex:any) {
      $log.info(`ex str: ${JSON.stringify(ex, null, 2)}`);
      $log.info(`message: ${ex.message}`);

      expect(ex.message).toMatch('test');
    }

    // charlie -> bondingCurve: buy
    $log.info(`charlie -> bondingCurve: buy`);
    const charlie_buy_op = await bonding_curve_charlie.methods.buy().send({ amount: 272, mutez: true });
    await charlie_buy_op.confirmation();


    // $log.info(`charlie -> bondingCurve: sell(3)`);
    // charlie -> bondingCurve: sell
    // parameter: 3

    // $log.info(`bob -> bondingCurve: sell(2)`);
    // bob -> bondingCurve: sell
    // parameter: 2

    // $log.info(`alice -> bondingCurve: sell(1)`);
    // alice -> bondingCurve: sell
    // parameter: 1

    // admin -> bondingCurve: withdraw
    // parameter: Unit

    expect('ok').toBe('ok');
  });


  // test('change admin by non admin should fail', async () => {
  //   const opSetAdmin = nftEditionsAlice.methods.set_admin(aliceAddress).send();
  //   return expect(opSetAdmin).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  // });

  // test('pause by non admin should fail', async () => {
  //   const opPause = nftEditionsAlice.methods.pause([true]).send();
  //   return expect(opPause).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  // });

  // test('change admin by admin should succeed', async () => {
  //   $log.info("Testing change admin");
  //   const opSetAdmin = await nftEditionsBob.methods.set_admin(aliceAddress).send();
  //   await opSetAdmin.confirmation();
  //   const opConfirmAdmin = await nftEditionsAlice.methods.confirm_admin(["unit"]).send();
  //   await opConfirmAdmin.confirmation();
  //   const contractStorage1 : any = await nftEditionsBob.storage();
  //   const admin = await contractStorage1.nft_asset_storage.admin.admin;
  //   expect(admin).toEqual(aliceAddress);
  //   $log.info("Admin changed successfully");

  //   $log.info("Set admin back");
  //   const opSetAdminBack = await nftEditionsAlice.methods.set_admin(bobAddress).send();
  //   await opSetAdminBack.confirmation();
  //   const opConfirmAdminBob = await nftEditionsBob.methods.confirm_admin(["unit"]).send();
  //   await opConfirmAdminBob.confirmation();
  //   const contractStorage2 : any = await nftEditionsBob.storage();
  //   const finalAdmin = await contractStorage2.nft_asset_storage.admin.admin;
  //   expect(finalAdmin).toEqual(bobAddress);
  //   $log.info("Admin changed back successfully");
  // });

  // // test('minting by non admin should fail', async () => {
  // //   const nft = {
  // //     edition_info: edition_1_metadata,
  // //     number_of_editions: new BigNumber(1000),
  // //   };
  // //   const opMint = nftEditionsAlice.methods.mint_editions([nft]).send();
  // //   return expect(opMint).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  // // });

  // // test('minting too large of an edition set should fail', async () => {
  // //   const nft = {
  // //     edition_info: edition_1_metadata,
  // //     number_of_editions: maxEditions.plus(1),
  // //   };
  // //   const opMint = nftEditionsBob.methods.mint_editions([nft]).send();
  // //   return expect(opMint).rejects.toHaveProperty('message', 'EDITION_RUN_TOO_LARGE');
  // // });

  // // NOTE: needs to be run synchronously, tests that follow depend on these editions having been minted
  // test('mint 1000 editions of nft1 and 2 of nft2', async () => {
  //   nft1 = {
  //     edition_info: edition_1_metadata,
  //     number_of_editions: new BigNumber(1000),
  //   };

  //   nft2 = {
  //     edition_info: edition_2_metadata,
  //     number_of_editions: new BigNumber(2),
  //   };
  //   const opMint = await nftEditionsBob.methods.mint_editions([nft1, nft2]).send();
  //   await opMint.confirmation();
  //   $log.info(`Minted editions. Consumed gas: ${opMint.consumedGas}`);
  // });

  // test('distribute editions', async () => {
  //   const distributeEdition0: distribute_edition = {
  //     edition_id: new BigNumber(0),
  //     receivers: [aliceAddress, bobAddress],
  //   };
  //   const distributeEdition1: distribute_edition = {
  //     edition_id: new BigNumber(1),
  //     receivers: [aliceAddress, bobAddress],
  //   };
  //   const opDistribute = await nftEditionsBob.methods
  //     .distribute_editions([distributeEdition0, distributeEdition1]).send();
  //   await opDistribute.confirmation();
  //   $log.info(`Distributed editions. Consumed gas: ${opDistribute.consumedGas}`);

  //   const [aliceHasEdition0, bobHasEdition0] = await hasTokens([
  //     { owner: aliceAddress, token_id: new BigNumber(0) },
  //     { owner: bobAddress, token_id: new BigNumber(1) },
  //   ], queryBalances, nftEditionsBob);

  //   const [aliceHasEdition1, bobHasEdition1] = await hasTokens([
  //     { owner: aliceAddress, token_id: maxEditions },
  //     { owner: bobAddress, token_id: maxEditions.plus(1) },
  //   ], queryBalances, nftEditionsBob);

  //   expect(aliceHasEdition0).toBe(true);
  //   expect(bobHasEdition0).toBe(true);
  //   expect(aliceHasEdition1).toBe(true);
  //   expect(bobHasEdition1).toBe(true);
  // });


  // test('distributing too many editions should fail', async () => {
  //   const distributeEdition1: distribute_edition = {
  //     edition_id: new BigNumber(1),
  //     receivers: [aliceAddress],
  //   };
  //   const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition1]).send();
  //   return expect(opDistribute).rejects.toHaveProperty('message', 'NO_EDITIONS_TO_DISTRIBUTE');
  // });
  // test('distributing from a 0 edition set should fail', async () => {
  //   const nft3 = {
  //     edition_info: edition_1_metadata,
  //     number_of_editions: new BigNumber(0),
  //   };
  //   const opMint = await nftEditionsBob.methods.mint_editions([nft3]).send();
  //   await opMint.confirmation();
  //   $log.info(`Minted editions. Consumed gas: ${opMint.consumedGas}`);
  //   const distributeEdition3: distribute_edition = {
  //     edition_id: new BigNumber(2),
  //     receivers: [aliceAddress],
  //   };
  //   const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition3]).send();
  //   return expect(opDistribute).rejects.toHaveProperty('message', 'NO_EDITIONS_TO_DISTRIBUTE');
  // });

  // test('distributing exactly as many editions available should succeed with 0 editions left to distribute',
  //      async () => {
  //   const nft4 = {
  //     edition_info: edition_1_metadata,
  //     number_of_editions: new BigNumber(3),
  //   };
  //   const opMint = await nftEditionsBob.methods.mint_editions([nft4]).send();
  //   await opMint.confirmation();
  //   $log.info(`Minted editions. Consumed gas: ${opMint.consumedGas}`);
  //   const distributeEdition3: distribute_edition = {
  //     edition_id: new BigNumber(3),
  //     receivers: [aliceAddress, aliceAddress, aliceAddress],
  //   };
  //   const opDistribute = await nftEditionsBob.methods.distribute_editions([distributeEdition3]).send();
  //   await opDistribute.confirmation();
  //   const editions_storage : any = await nftEditionsBob.storage();
  //   const editions_metadata = await editions_storage.editions_metadata.get("3");
  //   expect(JSON.stringify(editions_metadata.number_of_editions_to_distribute, null, 2)).toEqual("\"0\"");
  // });

  // test('transfer edition', async () => {
  //   const tokenId = new BigNumber(0);
  //   const nat1 = new BigNumber(1);
  //   await transfer(nftEditionsBob.address, tezos.alice, [
  //     {
  //       from_: aliceAddress,
  //       txs: [{ to_: bobAddress, token_id: tokenId, amount: nat1 }],
  //     },
  //   ]);
  //   const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
  //     { owner: aliceAddress, token_id: tokenId },
  //     { owner: bobAddress, token_id: tokenId },
  //   ], queryBalances, nftEditionsBob);
  //   expect(aliceHasATokenAfter).toBe(false);
  //   expect(bobHasATokenAfter).toBe(true);
  // });

  // test('transfer edition that does not exist should fail', async () => {
  //   const tokenId = new BigNumber(1000); //this token should not exist
  //   const nat1 = new BigNumber(1);
  //   const opTransfer = transfer(nftEditionsBob.address, tezos.alice, [
  //     {
  //       from_: aliceAddress,
  //       txs: [{ to_: bobAddress, token_id: tokenId, amount: nat1 }],
  //     },
  //   ]);
  //   return expect(opTransfer).rejects.toHaveProperty('message', 'FA2_TOKEN_UNDEFINED');
  // });

  // test('test editions token-metadata with off-chain view', async () => {
  //   tezos.bob.addExtension(new Tzip16Module());
  //   const editionsContractMetadata = await tezos.bob.contract.at(nftEditionsBob.address, tzip16);
  //   $log.info(`Initialising the views for editions contract ...`);
  //   const views = await editionsContractMetadata.tzip16().metadataViews();
  //   $log.info(`The following view names were found in the metadata: ${Object.keys(views)}`);
  //   $log.info(`get metadata for edition with token_id 0 ...`);
  //   const token0Metadata = await views.token_metadata().executeView(0);
  //   expect(token0Metadata).toEqual({
  //     token_id: new BigNumber(0),
  //     token_info: edition_1_metadata,
  //   });
  // });

  // test('Distributing from an edition set you did not create should fail', async () => {
  //   const distributeEdition0: distribute_edition = {
  //     edition_id: new BigNumber(0),
  //     receivers: [aliceAddress],
  //   };
  //   const opDistribute = nftEditionsAlice.methods.distribute_editions([distributeEdition0]).send();
  //   return expect(opDistribute).rejects.toHaveProperty('message', 'INVALID_DISTRIBUTOR');
  // });

  // test('Distributing from a non existing edition set should fail', async () => {
  //   const distributeEdition5: distribute_edition = {
  //     edition_id: new BigNumber(5),
  //     receivers: [aliceAddress],
  //   };
  //   const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition5]).send();
  //   return expect(opDistribute).rejects.toHaveProperty('message', 'INVALID_EDITION_ID');
  // });

  // test('Distributing while contract is paused should fail', async () => {
  //   $log.info("pausing the contract");
  //   const opPause = await nftEditionsBob.methods.pause([true]).send();
  //   await opPause.confirmation();
  //   $log.info("contract paused");
  //   const distributeEdition0: distribute_edition = {
  //     edition_id: new BigNumber(0),
  //     receivers: [aliceAddress],
  //   };
  //   const opDistribute = nftEditionsBob.methods.distribute_editions([distributeEdition0]).send();
  //   return expect(opDistribute).rejects.toHaveProperty('message', 'PAUSED');
  // });

});
