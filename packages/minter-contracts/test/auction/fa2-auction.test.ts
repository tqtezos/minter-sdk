import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import {
  MintNftParam,
  originateFtFaucet,
  originateNftFaucet,
  originateEnglishAuctionFA2,
  MintFtParam,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';
import { queryBalancesWithLambdaView, getBalances, QueryBalances, hasTokens } from '../../test/fa2-balance-inspector';

jest.setTimeout(300000); // 5 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
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
  let queryBalances : QueryBalances;

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
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
  });

  beforeEach(async() =>{
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
    nftAuction = await originateEnglishAuctionFA2(tezos.bob, ftContract.address, tokenIdBidToken);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
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

    const fa2_token : Fa2_token = {
      token_id : tokenIdNft,
      amount : new BigNumber(1),
    };

    const tokens : Tokens = {
      fa2_address : nftContract.address,
      fa2_batch : [fa2_token],
    };

    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(1, 'minute').toDate();
    $log.info(`Configuring auction`);
    const opAuction = await nftAuctionBob.methods.configure(
      //opening price = 1
      new BigNumber(10),
      //percent raise =10
      new BigNumber(10),
      //min_raise = 10
      new BigNumber(10),
      //round_time = 1 hr
      new BigNumber(3600),
      //extend_time = 0 seconds
      new BigNumber(0),
      //asset
      [tokens],
      //start_time = now + 7seconds
      startTime,
      //end_time = start_time + 1hr,
      endTime,
    ).send({ amount : 0 });
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
  });

  test('NFT is held by the auction contract after configuration', async() => {
    const [auctionHasNft] = await hasTokens([
      { owner: nftAuction.address, token_id: tokenIdNft },
    ], queryBalances, nftContract);
    expect(auctionHasNft).toBe(true);
  });

  test('bid of less than asking price should fail', async() => {
    $log.info(`Alice bids 9 tokens expecting it to fail`);
    const failedOpeningBid = nftAuctionAlice.methods.bid(0, 9).send({ amount : 0 });
    //TODO: test contents of error message
    return expect(failedOpeningBid).rejects.toHaveProperty('errors');
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise_percent', async () => {
    $log.info(`Alice bids 10 token`);
    const opBid = await nftAuctionAlice.methods.bid(0, 10).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed`);

    $log.info(`Alice bids 11 tokens`);
    const opBid2 = await nftAuctionAlice.methods.bid(0, 11).send({ amount : 0 });
    await opBid2.confirmation();
    $log.info(`Bid placed`);
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise', async () => {
    $log.info(`Alice bids 200 tokens`);
    const opBid = await nftAuctionAlice.methods.bid(0, 200).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed`);

    $log.info(`Alice bids 210 tokens, a 10 token increase but less than a 10% raise of previous bid `);
    const opBid2 = await nftAuctionAlice.methods.bid(0, 210).send({ amount : 0 });
    await opBid2.confirmation();
    $log.info(`Bid placed.`);
  });
  test('bid too small should fail', async () => {
    $log.info(`Alice bids 20  tokens`);
    const opBid = await nftAuctionAlice.methods.bid(0, 20).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed`);
    $log.info(`Alice bids 21 tokens and we expect it to fail`);
    const smallBidPromise = nftAuctionAlice.methods.bid(0, 21).send({ amount : 0 });
    return expect(smallBidPromise).rejects.toHaveProperty('errors' );
  });

  test('auction without bids that is cancelled should only return asset', async () => {
    const [aliceBalanceBefore, bobBalanceBefore] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Alices's balance is ${aliceBalanceBefore.toNumber()} and Bob's is ${bobBalanceBefore.toNumber()} `);
    $log.info("Cancelling auction");
    const opCancel = await nftAuctionBob.methods.cancel(0).send({ amount : 0, mutez : true });
    await opCancel.confirmation();
    $log.info("Auction cancelled");
    const [aliceBalanceAfter, bobBalanceAfter] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceAfter.toNumber()} and Alice's is ${aliceBalanceAfter.toNumber()}`);
    if (aliceBalanceBefore.eq(aliceBalanceAfter) && bobBalanceBefore.eq(bobBalanceAfter)) $log.info("Bids returned as expected");
    else throw new Error(`FA2 returned incorrectly`);
    const [sellerHasNft, auctionHasNft] = await hasTokens([
      { owner: bobAddress, token_id: tokenIdNft },
      { owner: nftAuction.address, token_id: tokenIdNft },
    ], queryBalances, nftContract);
    expect(sellerHasNft).toBe(true);
    expect(auctionHasNft).toBe(false);
    $log.info(`NFT returned as expected`);
  });
  test('auction with bid that is cancelled should return asset and bid', async () => {
    const [aliceBalanceBefore, bobBalanceBefore] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceBefore.toNumber()} and Alice's is ${aliceBalanceBefore.toNumber()}`);
    $log.info(`Alice bids 200 tokens`);
    const alicesBid = new BigNumber(200);
    const opBid = await nftAuctionAlice.methods.bid(0, alicesBid.toNumber()).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed`);

    $log.info("Cancelling auction should return asset and highest bid");
    const opCancel = await nftAuctionBob.methods.cancel(0).send({ amount : 0, mutez : true });
    await opCancel.confirmation();
    $log.info("Auction cancelled");
    const [aliceBalanceAfter, bobBalanceAfter] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);

    $log.info(`Bob's balance is ${bobBalanceAfter.toNumber()} and Alice's is ${aliceBalanceAfter.toNumber()}`);
    if (aliceBalanceBefore.eq(aliceBalanceAfter) && bobBalanceBefore.eq(bobBalanceAfter)) $log.info("Bids returned as expected");
    else throw new Error(`FA2 returned incorrectly`);
    const [sellerHasNft, auctionHasNft] = await hasTokens([
      { owner: bobAddress, token_id: tokenIdNft },
      { owner: nftAuction.address, token_id: tokenIdNft },
    ], queryBalances, nftContract);
    expect(sellerHasNft).toBe(true);
    expect(auctionHasNft).toBe(false);
    $log.info(`NFT returned as expected`);
  });
  test('resolved auction should send asset and return highest bid', async () => {
    const [aliceBalanceBefore, bobBalanceBefore] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceBefore.toNumber()} and Alice's is ${aliceBalanceBefore.toNumber()}`);
    $log.info(`Alice bids 200 tokens`);
    const alicesBid = new BigNumber(200);
    const opBid = await nftAuctionAlice.methods.bid(0, alicesBid.toNumber()).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed`);
    await sleep(90000); //90 seconds

    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount : 0 });
    await opResolve.confirmation();
    $log.info("Auction Resolve");

    const [aliceBalanceAfter, bobBalanceAfter] = await getBalances([
      { owner: aliceAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceAfter.toNumber()} and Alice's is ${aliceBalanceAfter.toNumber()}`);
    if (aliceBalanceBefore.eq(aliceBalanceAfter) && bobBalanceBefore.eq(bobBalanceAfter)) $log.info("Bids returned as expected");
    else throw new Error(`Bids returned incorrectly`);

    const [sellerHasNft, auctionHasNft, buyerHasNft] = await hasTokens([
      { owner: bobAddress, token_id: tokenIdNft },
      { owner: nftAuction.address, token_id: tokenIdNft },
      { owner: aliceAddress, token_id: tokenIdNft },
    ], queryBalances, nftContract);
    expect(sellerHasNft).toBe(false);
    expect(auctionHasNft).toBe(false);
    expect(buyerHasNft).toBe(true);
    $log.info(`NFT returned as expected`);
  });
});
