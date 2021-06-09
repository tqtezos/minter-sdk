import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, bytes, address } from '../../src/type-aliases';
import { InternalOperationResult } from '@taquito/rpc';
import {
  originateEnglishAuctionTezFixedFee,
  MintNftParam,
  originateNftFaucet,
} from '../../src/nft-contracts';
import { MichelsonMap } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';

jest.setTimeout(180000); // 3 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob: Contract;
  let nftAuctionAlice: Contract;
  let nftContract: Contract;
  let bobAddress: address;
  let eveAddress: address;
  let startTime: Date;
  let endTime: Date;
  let tokenId: BigNumber;
  let empty_metadata_map: MichelsonMap<string, bytes>;
  let mintToken: MintNftParam;

  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenId = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    eveAddress = await tezos.eve.signer.publicKeyHash();
    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map,
      },
      owner: bobAddress,
    };
  });

  beforeEach(async () => {
    $log.info('originating nft auction...');
    nftAuction = await originateEnglishAuctionTezFixedFee(tezos.bob, eveAddress);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);

    $log.info('originating nft faucets...');
    nftContract = await originateNftFaucet(tezos.bob);

    $log.info('minting token');

    const opMint = await nftContract.methods.mint([mintToken]).send();
    await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');

    const fa2_token: Fa2_token = {
      token_id: tokenId,
      amount: new BigNumber(1),
    };

    const tokens: Tokens = {
      fa2_address: nftContract.address,
      fa2_batch: [fa2_token],
    };

    startTime = moment.utc().add(7, 'seconds').toDate();
    endTime = moment(startTime).add(30, 'seconds').toDate();
    const opAuction = await nftAuctionBob.methods.configure(
      //opening price = 10 tz
      new BigNumber(10000000),
      //percent raise =10
      new BigNumber(10),
      //min_raise = 10tz
      new BigNumber(10000000),
      //round_time = 1 hr
      new BigNumber(3600),
      //extend_time = 0
      new BigNumber(0),
      //assset
      [tokens],
      //start_time = now + 7seconds
      startTime,
      //end_time = start_time + 5 seconds,
      endTime,
    ).send({ amount: 0 });
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
    await sleep(7000); //7 seconds
  });
  test('resolved auction should send contract fee', async () => {
    $log.info(`Alice bids 200tz`);
    const bidMutez = 200000000;
    const feePercent = 0.10;
    const opBid = await nftAuctionAlice.methods.bid(0).send({ amount: bidMutez, mutez: true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    await sleep(30000); //30 seconds

    $log.info("Resolving auction");
    const opResolve = await nftAuctionBob.methods.resolve(0).send({ amount: 0 });
    await opResolve.confirmation();
    $log.info("Auction resolved");

    const internalOps = opResolve.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    const paymentAmount = internalOps[0].amount;
    const paymentReceiver = internalOps[0].destination;
    const feeAmount = internalOps[1].amount;
    const feeReceiver = internalOps[1].destination;

    if (feeAmount !== (bidMutez * feePercent).toString() && feeReceiver !== eveAddress)
      throw new Error(`Operation incorrectly resulted in a transfer of ${feeAmount} to ${feeReceiver} for fee`);
    else $log.info(`${feeAmount} correctly transferred to ${feeReceiver} for fee`);

    if (paymentAmount !== (bidMutez * (1 - feePercent)).toString() && paymentReceiver !== bobAddress)
      throw new Error(`Operation incorrectly resulted in a transfer of ${paymentAmount} to ${paymentReceiver} for payment`);
    else $log.info(`${paymentAmount} correctly transferred to ${paymentReceiver} for payment`);

  });
});
