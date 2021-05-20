import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import {
  MintNftParam,
  originateFtFaucet,
  originateNftFaucet,
  MintFtParam,
  originateEnglishAuctionFA2Admin,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens } from '../../src/auction-interface';


jest.setTimeout(300000); // 5 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionAlice : Contract;
  let nftContract : Contract;
  let ftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
  let startTime : Date;
  let endTime : Date;
  let tokenIdNft : BigNumber;
  let tokenIdBidToken : BigNumber;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let nft : MintNftParam;
  let bid_tokens_bob : MintFtParam;
  let bid_tokens_alice : MintFtParam;


  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenIdNft = new BigNumber(1);
    tokenIdBidToken = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    nft = {
      token_metadata: {
        token_id: tokenIdNft,
        token_info: empty_metadata_map,
      },
      owner: bobAddress,
    };
    bid_tokens_bob = {
      token_id: tokenIdBidToken,
      owner: bobAddress,
      amount : new BigNumber(300),
    };
    bid_tokens_alice = {
      token_id: tokenIdBidToken,
      owner: aliceAddress,
      amount : new BigNumber(300),
    };
    $log.info('originating nft faucet...');
    nftContract = await originateNftFaucet(tezos.bob);

    $log.info('originating ft faucet...');
    ftContract = await originateFtFaucet(tezos.bob);

    $log.info('minting nft');
    const opMintNft = await nftContract.methods.mint([nft]).send();
    await opMintNft.confirmation();
    $log.info(`Minted nft. Consumed gas: ${opMintNft.consumedGas}`);

    $log.info('creating fa2 for bids');
    const opCreateFA2 = await ftContract.methods.create_token(tokenIdBidToken, empty_metadata_map).send();
    await opCreateFA2.confirmation();
    $log.info(`Created FA2 Consumed gas: ${opCreateFA2.consumedGas}`);

    $log.info('minting fa2 for bids');
    const opMintFA2 = await ftContract.methods.mint_tokens([bid_tokens_alice, bid_tokens_bob]).send();
    await opMintFA2.confirmation();
    $log.info(`Minted FA2. Consumed gas: ${opMintFA2.consumedGas}`);

    $log.info('originating nft auction with payment in FA2...');
    nftAuction = await originateEnglishAuctionFA2Admin(tezos.bob, bobAddress, ftContract.address, tokenIdBidToken);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);

    $log.info('adding auction contract as operator for nft');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenIdNft);
    $log.info('Auction contract added as operator for nft');

    $log.info('adding auction contract as operator for bid token for alice');
    await addOperator(ftContract.address, tezos.alice, nftAuction.address, tokenIdBidToken);
    $log.info('Auction contract added as operator for bid token');

    $log.info('adding auction contract as operator for bid token for bob');
    await addOperator(ftContract.address, tezos.bob, nftAuction.address, tokenIdBidToken);
    $log.info('Auction contract added as operator for bid token for bob');
  });
  test('configuration not from admin should fail', async() => {
    const fa2_token : Fa2_token = {
      token_id : tokenIdNft,
      amount : new BigNumber(1),
    };

    const tokens : Tokens = {
      fa2_address : nftContract.address,
      fa2_batch : [fa2_token],
    };
    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(1, 'hours').toDate();
    $log.info(`Alice attempts to configure auction, we expect it to fail`);
    const opAuctionPromise = nftAuctionAlice.methods.configure(
      //opening price = 1
      new BigNumber(10),
      //percent raise =10
      new BigNumber(10),
      //min_raise = 10
      new BigNumber(10),
      //round_time = 1 hr
      new BigNumber(3600),
      //extend_time = 5 mins
      new BigNumber(300),
      //asset
      [tokens],
      //start_time = now + 7seconds
      startTime,
      //end_time = start_time + 1hr,
      endTime,
    ).send({ amount : 0 });
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
