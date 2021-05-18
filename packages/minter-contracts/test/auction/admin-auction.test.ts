import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import moment from 'moment';
import {
  originateEnglishAuctionTezAdmin,
  MintNftParam,
  originateNftFaucet,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens } from '../../src/auction-interface';

jest.setTimeout(180000); // 3 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionAlice : Contract;
  let nftContract : Contract;
  let aliceAddress : address;
  let bobAddress : address;
  let startTime : Date;
  let endTime : Date;
  let empty_metadata_map: MichelsonMap<string, bytes>;
  let tokenId : BigNumber;
  let mintToken : MintNftParam;
  let fa2_token : Fa2_token;
  let auction_tokens : Tokens;

  beforeAll(async () => {
    tezos = await bootstrap();
    nftContract = await originateNftFaucet(tezos.alice);
    $log.info('originating nft auction with eve as admin...');
    nftAuction = await originateEnglishAuctionTezAdmin(tezos.bob);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);

    $log.info('minting token');

    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    empty_metadata_map = new MichelsonMap();

    tokenId = new BigNumber(0);

    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map,
      },
      owner: aliceAddress,
    };
    const opMint = await nftContract.methods.mint([mintToken]).send();
    await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.alice, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');

    fa2_token = {
      token_id : tokenId,
      amount : new BigNumber(1),
    };

    auction_tokens = {
      fa2_address : nftContract.address,
      fa2_batch : [fa2_token],
    };
  });
  test('configuration not from admin should fail', async() => {
    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(1, 'hours').toDate();
    $log.info(`Alice attempts to configure auction, we expect it to fail`);
    const opAuctionPromise = nftAuctionAlice.methods.configure(
      //opening price = 10 tz
      new BigNumber(10000000),
      //percent raise =10
      new BigNumber(10),
      //min_raise = 10tz
      new BigNumber(10000000),
      //round_time = 1 hr
      new BigNumber(3600),
      //extend_time = 5 mins
      new BigNumber(300),
      //assset
      [auction_tokens],
      //start_time = now + 7seconds
      startTime,
      //end_time = start_time + 1hr,
      endTime,
    ).send({ amount : 10 });
    return expect(opAuctionPromise).rejects.toHaveProperty('message', "NOT_AN_ADMIN");
  });

  test('pause by non admin should fail', async () => {
    const opPause = nftAuctionAlice.methods.pause([true]).send();
    return expect(opPause).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

  test('change admin by admin should succeed', async () => {
    $log.info("Testing change admin");
    const opSetAdmin = await nftAuction.methods.set_admin(aliceAddress).send();
    await opSetAdmin.confirmation();
    const opConfirmAdmin = await nftAuctionAlice.methods.confirm_admin(["unit"]).send();
    await opConfirmAdmin.confirmation();
    const contractStorage1 : any = await nftAuction.storage();
    const admin = await contractStorage1.admin.admin;
    expect(admin).toEqual(aliceAddress);
    $log.info("Admin changed successfully");

    $log.info("Set admin back");
    const opSetAdminBack = await nftAuctionAlice.methods.set_admin(bobAddress).send();
    await opSetAdminBack.confirmation();
    const opConfirmAdminBob = await nftAuction.methods.confirm_admin(["unit"]).send();
    await opConfirmAdminBob.confirmation();
    const contractStorage2 : any = await nftAuction.storage();
    const finalAdmin = await contractStorage2.admin.admin;
    expect(finalAdmin).toEqual(bobAddress);
    $log.info("Admin changed back successfully");
  });

  test('change admin by non admin should fail', async () => {
    const opSetAdmin = nftAuctionAlice.methods.set_admin(aliceAddress).send();
    return expect(opSetAdmin).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

});
