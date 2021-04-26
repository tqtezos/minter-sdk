import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import {
  MintNftParam,
  originateFtFaucet,
  originateNftFaucet,
  originateEnglishAuctionFA2FixedFee,
  MintFtParam,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';
import { queryBalancesWithLambdaView, getBalances, QueryBalances } from '../../test/fa2-balance-inspector';

jest.setTimeout(240000); // 4 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
  let nftAuctionAlice : Contract;
  let nftContract : Contract;
  let ftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
  let eveAddress : address;
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
    eveAddress = await tezos.eve.signer.publicKeyHash();
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
    nftAuction = await originateEnglishAuctionFA2FixedFee(tezos.bob, eveAddress, ftContract.address, tokenIdBidToken);
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
    endTime = moment(startTime).add(30, 'seconds').toDate();
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
      //end_time = start_time + 5 seconds,
      endTime,
    ).send({ amount : 0 });
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
  });
  test('resolved auction should send contract fee', async () => {
    $log.info(`Alice bids 200tz`);
    const bidAmount = 200;
    const feePercent = 0.10;
    const opBid = await nftAuctionAlice.methods.bid(0, bidAmount).send();
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    await sleep(30000); //30 seconds

    const [eveBalanceBefore, bobBalanceBefore] = await getBalances([
      { owner: eveAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceBefore.toNumber()} and Eve's is ${eveBalanceBefore.toNumber()}`);

    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount: 0 });
    await opResolve.confirmation();
    $log.info("Auction resolved");

    const [eveBalanceAfter, bobBalanceAfter] = await getBalances([
      { owner: eveAddress, token_id: tokenIdBidToken },
      { owner: bobAddress, token_id: tokenIdBidToken },
    ], queryBalances, ftContract);
    $log.info(`Bob's balance is ${bobBalanceAfter.toNumber()} and Eve's is ${eveBalanceAfter.toNumber()}`);


    if (eveBalanceAfter.minus(eveBalanceBefore).eq(bidAmount * feePercent) &&
        bobBalanceAfter.eq(bobBalanceBefore.plus((1 - feePercent) * bidAmount))) {
      $log.info("Fee paid as expected upon auction resolution");
    } else {
      throw new Error(`Fee paid incorrectly upon auction resolution`);
    }

  });
});
