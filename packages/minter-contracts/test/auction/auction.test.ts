import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import { InternalOperationResult } from '@taquito/rpc';
import {
  originateEnglishAuctionTez,
  MintNftParam,
  originateNftFaucet,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';
import { queryBalancesWithLambdaView, hasTokens, QueryBalances } from '../../test/fa2-balance-inspector';

jest.setTimeout(300000); // 5 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
  let nftAuctionAlice : Contract;
  let nftAuctionEve : Contract;
  let nftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
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
    nftAuction = await originateEnglishAuctionTez(tezos.bob);
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
    endTime = moment(startTime).add(1, 'minute').toDate();
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
      //end_time = start_time + 5seconds,
      endTime,
    ).send({ amount : 0 });
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
    await sleep(7000); //7 seconds
  });

  test('NFT is held by the auction contract after configuration', async() => {
    const [auctionHasNft] = await hasTokens([
      { owner: nftAuction.address, token_id: tokenId },
    ], queryBalances, nftContract);
    expect(auctionHasNft).toBe(true);
  });
  test('bid of less than asking price should fail', async() => {
    $log.info(`Alice bids 9tz expecting it to fail`);
    const failedOpeningBid = nftAuctionAlice.methods.bid(0).send({ amount : 9 });
    //TODO: test contents of error message
    return expect(failedOpeningBid).rejects.toHaveProperty('errors');
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise_percent', async () => {
    $log.info(`Alice bids 10tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : 10 });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    $log.info(`Alice bids 11tz, a 10% raise of previous bid but less than a 10tz increase`);
    const opBid2 = await nftAuctionAlice.methods.bid(0).send({ amount : 11 });
    await opBid2.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid2.amount} mutez`);
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise', async () => {
    $log.info(`Alice bids 200tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : 200 });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    $log.info(`Eve bids 210tz, a 10tz increase but less than a 10% raise of previous bid `);
    const opBid2 = await nftAuctionEve.methods.bid(0).send({ amount : 210 });
    await opBid2.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid2.amount} mutez`);
  });
  test('bid too small should fail', async () => {
    $log.info(`Alice bids 20tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : 20 });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount}`);
    $log.info(`Alice bids 21tz and we expect it to fail`);
    const smallBidPromise = nftAuctionAlice.methods.bid(0).send({ amount : 21 });
    return expect(smallBidPromise).rejects.toHaveProperty('errors' );
  });

  test('auction without bids that is cancelled should not result in transfer of any tez', async () => {
    $log.info("Cancelling auction");
    const opCancel = await nftAuctionBob.methods.cancel(0).send({ amount : 0, mutez : true });
    await opCancel.confirmation();
    $log.info("Auction cancelled");
    const internalOps = (opCancel.operationResults[0].metadata.internal_operation_results as InternalOperationResult[]);

    expect(internalOps.length).toEqual(1);
    expect(internalOps[0].destination).toEqual(nftContract.address);
    $log.info("No amount in tez is sent as we expect");
  });

  test('auction with bids that is cancelled should return last bid', async () => {
    $log.info(`Alice bids 200tz`);
    const bidMutez = 200000000;
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : bidMutez, mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    $log.info("Cancelling auction");
    const opCancel = await nftAuctionBob.methods.cancel(0).send({ amount : 0 });
    await opCancel.confirmation();
    $log.info("Auction cancelled");
    const feeOp = (opCancel.operationResults[0].metadata.internal_operation_results as InternalOperationResult[])
      [0];
    const amountReturned = feeOp.amount;
    const feeDestination = feeOp.destination;
    expect(amountReturned).toEqual(bidMutez.toString());
    expect(feeDestination).toEqual(aliceAddress);
    $log.info(`${bidMutez.toString()}mutez returned to seller as expected`);
  });

  test('auction resolved before end time should fail', async () => {
    $log.info(`Alice bids 200tz`);
    const bidMutez = 200000000;
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : bidMutez, mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    $log.info("Resolving auction");
    const opResolve = nftAuctionBob.methods.resolve(0).send({ amount : 0 });
    expect(opResolve).rejects.toHaveProperty('errors');
    $log.info('Resolve operation failed as expected when called before end time');
  });

  test('auction without bids that is resolved after end time should only return asset to seller', async () => {
    await sleep(70000); //70 seconds
    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount : 0 });
    await opResolve.confirmation();
    const [sellerHasNft] = await hasTokens([
      { owner: bobAddress, token_id: tokenId },
    ], queryBalances, nftContract);
    expect(sellerHasNft).toBe(true);
    $log.info(`NFT returned as expected`);
    const internalOps = opResolve.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps.length).toEqual(1);
    expect(internalOps[0].destination).toEqual(nftContract.address);
  });

  test('auction cancelled after end time should fail', async () => {
    const bidMutez = new BigNumber(200000000);
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : bidMutez.toNumber(), mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed`);
    await sleep(70000); //70 seconds
    const opCancel = nftAuctionBob.methods.cancel(0).send({ amount : 0, mutez : true });
    expect(opCancel).rejects.toHaveProperty('errors');
    $log.info("Cancel after end time fails as expected");
  });

  test('resovled auction should send payment to seller and NFT to winning bidder', async () => {
    $log.info(`Alice bids 200tz`);
    const bidMutez = new BigNumber(200000000);
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount : bidMutez.toNumber(), mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    await sleep(70000); //70 seconds

    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount : 0 });
    await opResolve.confirmation();
    $log.info("Auction Resolve");
    const feeOp = (opResolve.operationResults[0].metadata.internal_operation_results as InternalOperationResult[])
      [0];
    const amountReturned = feeOp.amount;
    const feeDestination = feeOp.destination;
    expect(amountReturned).toEqual(bidMutez.toString());
    expect(feeDestination).toEqual(bobAddress);
    $log.info(`${bidMutez.toString()}tez sent to seller as expected`);
    const aliceAddress = await tezos.alice.signer.publicKeyHash();

    const [aliceHasNft] = await hasTokens([
      { owner: aliceAddress, token_id: tokenId },
    ], queryBalances, nftContract);
    expect(aliceHasNft).toBe(true);
  });
});
