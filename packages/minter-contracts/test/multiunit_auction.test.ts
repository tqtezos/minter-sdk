import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, bytes, address, nat, mutez, timestamp } from '../src/type-aliases';
import { InternalOperationResult } from '@taquito/rpc';
import {
  originateMultiunitAuctionOffchain,
  originateFtLimited, 
  mintLimitedFtTokens, 
  MintLimitedFtParam,
} from '../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';
import { addOperator } from '../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../src/auction-interface';
import { queryBalancesWithLambdaView, hasTokens, QueryBalances, getBalances } from './fa2-balance-inspector';
import { ThreeXBondingCurveCode } from '../bin-ts/three_x_bonding_curve.code'

jest.setTimeout(360000); // 6 minutes

export interface Asset {
  fa2_address : address;
  token_id : nat;
  amount_ : nat;
}

export interface BidParam {
  auction_id : nat;
  quantity : nat;
  price : mutez;
}

export interface ConfigureMultiunitAuctionParam {
    price_floor : mutez;
    round_time : nat;
    extend_time : nat;
    asset : Asset;
    start_time : Date;
    end_time : Date;
    bonding_curve : nat;
}

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
  let token_info_bob : MichelsonMap< string, string>;
  let nftContractAddress : address;
  let queryBalances : QueryBalances;
  let token0FixedSupply : nat;
  let tokensBob : MintLimitedFtParam;

  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenId = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
  });

  beforeEach(async() => {
    $log.info('originating nft auction...');
    nftAuction = await originateMultiunitAuctionOffchain(tezos.bob);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);
    nftAuctionEve = await tezos.eve.contract.at(nftAuction.address);
    
    $log.info('Adding sample bonding curve');
    const bonding_curve = ThreeXBondingCurveCode.code;
    $log.info(bonding_curve);
    const opAddBondingCurve = await nftAuction.methods.add_bonding_curve(bonding_curve).send();
    await opAddBondingCurve.confirmation();
    $log.info(opAddBondingCurve.operationResults[0].parameters?.value);
    $log.info('Sample bonding curve added');

    nftContract = await originateFtLimited(tezos.bob, bobAddress);
    nftContractAddress = nftContract.address;
    token0FixedSupply = new BigNumber(1000000);
    tokensBob = {
      owner : bobAddress,
      amount : token0FixedSupply,
      token_info : empty_metadata_map,
    };

    $log.info('minting token');
    await mintLimitedFtTokens(nftContract, [tokensBob]);
    $log.info('token minted');

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');
    
    
    const asset : Asset = {
      fa2_address : nftContractAddress,
      token_id : tokenId,
      amount_ : token0FixedSupply,
    }

    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(90, 'seconds').toDate();

    const configureParam : ConfigureMultiunitAuctionParam = {
      //price_floor starts at 1 mutez (not 1 tez)
      price_floor : new BigNumber(1),
      //round_time = 1 hr
      round_time : new BigNumber(3600),
      //extend_time = 0 seconds
      extend_time : new BigNumber(0),
      //assset
      asset : asset,
      //start_time = now + 7seconds
      start_time : startTime,
      //end_time = start_time + 90 seconds,
      end_time : endTime,
      //bonding_curve = 0n index
      bonding_curve : new BigNumber(0),
    }

    $log.info('configuring auction');
    const opAuction = await nftAuctionBob.methodsObject.configure(configureParam).send();
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
    await sleep(7000); //7 seconds
  });

  /*
  test('NFT is held by the auction contract after configuration', async() => {

  const [auctionTokenBalance] = await getBalances([
    { owner: nftAuction.address, token_id: tokenId },
  ], queryBalances, nftContract);

  expect(auctionTokenBalance.isEqualTo(token0FixedSupply)).toBe(true);
  });
  */

  test('Bid of 1 2 2 3 has correct result', async() => {
    const auction_id = new BigNumber(0);
    $log.info('bid 1....');
    const bid1 : BidParam = {
      auction_id : auction_id,
      quantity : new BigNumber(1),
      price : new BigNumber(1000000) //1 tez
    }
    const bid1Mutez = new BigNumber(1000000);
    const opBid1 = await nftAuctionAlice.methodsObject.bid(bid1).send({ amount : bid1Mutez.toNumber(), mutez : true });
    $log.info(opBid1.operationResults[0].metadata.operation_result);
    await opBid1.confirmation();
    
    $log.info('bid 2....');
    const bid2 : BidParam = {
      auction_id : auction_id,
      quantity : new BigNumber(2),
      price : new BigNumber(2000000) //2 tez
    }
    const bid2Mutez = new BigNumber(4000000); //4 tez
    const opBid2 = await nftAuctionAlice.methodsObject.bid(bid2).send({ amount : bid2Mutez.toNumber(), mutez : true });
    $log.info(opBid2.operationResults[0].metadata.operation_result);
    await opBid2.confirmation();
    
    $log.info('bid 3 ....');
    const bid3 : BidParam = {
      auction_id : auction_id,
      quantity : new BigNumber(1),
      price : new BigNumber(3000000) //3 tez
    }
    const bid3Mutez = new BigNumber(3000000); //3 tez
    const opBid3 = await nftAuctionEve.methodsObject.bid(bid3).send({ amount : bid3Mutez.toNumber(), mutez : true });
    $log.info(opBid3.operationResults[0].metadata.operation_result);
    await opBid3.confirmation();
    
    const returnParam = [auction_id, new BigNumber(1)];

    $log.info('returning old bids ....');
    const opReturn = await nftAuctionBob.methodsObject.return_old_bids(returnParam).send();
    await opReturn.confirmation();
    $log.info(opReturn.results)
    
    await sleep(90000); //90 seconds

    const opResolve = await nftAuctionBob.methods.resolve(auction_id).send();
    await opResolve.confirmation();
    
    $log.info('paying out winners....');
    
    const payoutParam = [auction_id, new BigNumber(3)];
    const payWinners = await nftAuctionBob.methodsObject.payout_winners(payoutParam).send();
    await payWinners.confirmation();
    $log.info(payWinners.results);

  });

});
