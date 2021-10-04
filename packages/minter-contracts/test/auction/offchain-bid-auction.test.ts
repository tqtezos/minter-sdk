import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import { InternalOperationResult } from '@taquito/rpc';
import {
  originateEnglishAuctionTezOffchainBid,
  MintNftParam,
  originateNftFaucet,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';
import { queryBalancesWithLambdaView, hasTokens, QueryBalances } from '../../test/fa2-balance-inspector';

jest.setTimeout(360000); // 6 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
  let nftAuctionAlice : Contract;
  let nftAuctionEve : Contract;
  let nftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
  let eveAddress : address;
  let startTime : Date;
  let endTime : Date;
  let tokenId : BigNumber;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let mintToken : MintNftParam;
  let queryBalances : QueryBalances;

  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenId = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    eveAddress = await tezos.eve.signer.publicKeyHash();
    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map,
      },
      owner: bobAddress,
    };
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
  });

  beforeEach(async() => {
    $log.info('originating nft auction...');
    nftAuction = await originateEnglishAuctionTezOffchainBid(tezos.bob);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);
    nftAuctionEve = await tezos.eve.contract.at(nftAuction.address);

    $log.info('originating nft faucets...');
    nftContract = await originateNftFaucet(tezos.bob);

    $log.info('minting token');

    const opMint = await nftContract.methods.mint([mintToken]).send();
    await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');

    const fa2_token : Fa2_token = {
      token_id : tokenId,
      amount : new BigNumber(1),
    };

    const tokens : Tokens = {
      fa2_address : nftContract.address,
      fa2_batch : [fa2_token],
    };

    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(90, 'seconds').toDate();
    const opAuction = await nftAuctionBob.methods.configure(
      //opening price = 10 tz
      new BigNumber(10000000),
      //percent raise =10
      new BigNumber(10),
      //min_raise = 10tz
      new BigNumber(10000000),
      //round_time = 1 hr
      new BigNumber(3600),
      //extend_time = 0 seconds
      new BigNumber(0),
      //assset
      [tokens],
      //start_time = now + 7seconds
      startTime,
      //end_time = start_time + 90 seconds,
      endTime,
    ).send({ amount : 0 });
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
    await sleep(7000); //7 seconds
  });

  test('resovled auction should only send NFT to winning bidder, not send payment', async () => {
    const aliceAddress = await tezos.alice.signer.publicKeyHash();
    $log.info(`Alice bids 200tz`);
    const bidMutez = new BigNumber(0);
    const opBid = await nftAuctionBob.methods
      .offchain_bid(0, 10000000, aliceAddress).send({ amount : bidMutez.toNumber(), mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    await sleep(90000); //90 seconds

    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount : 0 });
    await opResolve.confirmation();
    $log.info("Auction Resolve");

    const [aliceHasNft] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
    ], queryBalances, nftContract);
    expect(aliceHasNft).toBe(true);
    $log.info(`NFT sent to winning bidder as expected`);
    const internalOps = opResolve.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps.length).toEqual(1);
    expect(internalOps[0].destination).toEqual(nftContract.address);
  });

  test('outbid offchain bid should not return offchain bid', async () => {
    const aliceAddress = await tezos.alice.signer.publicKeyHash();
    $log.info(`Alice bids 200tz`);
    const bidMutez = new BigNumber(0);
    const opBid = await nftAuctionBob.methods
      .offchain_bid(0, 10000000, aliceAddress).send({ amount : bidMutez.toNumber(), mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    const outBidMutez = new BigNumber(200000000);
    const opOutbid = await nftAuctionEve.methods.bid(0).send({ amount : outBidMutez.toNumber(), mutez : true });
    await opOutbid.confirmation();
    $log.info(`Bid placed`);

    const internalOps = opOutbid.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps).toBeUndefined();
  });


});
