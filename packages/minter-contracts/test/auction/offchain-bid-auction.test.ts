import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, address, bytes, nat, mutez, key } from '../../src/type-aliases';
import { InternalOperationResult } from '@taquito/rpc';
import {
  originateEnglishAuctionTezOffchainBid,
  MintNftParam,
  originateNftFaucet,
} from '../../src/nft-contracts';
import { ContractAbstraction, ContractProvider, MichelsonMap, TezosToolkit } from '@taquito/taquito';

import { addOperator } from '../../src/fa2-interface';
import { Fa2_token, Tokens, sleep } from '../../src/auction-interface';
import { queryBalancesWithLambdaView, hasTokens, QueryBalances } from '../../test/fa2-balance-inspector';

import {
  errors_to_missigned_bytes,
  Permit,
  dummy_sig,
} from '../../src/permit';

jest.setTimeout(360000); // 6 minutes

interface OffchainBidData {
  asset_id : nat;
  bid_amount : mutez;
}

interface PermitBidParam {
  offchain_bid_data : OffchainBidData;
  permit : Permit;
}

async function createPermit( signerKey : key,
  assetId: nat,
  bidAmount : mutez,
  auction : ContractAbstraction<ContractProvider>,
  permitSigner : TezosToolkit,
  submitter : TezosToolkit ): Promise<PermitBidParam> {
  const fake_permit : Permit = {
    signerKey : signerKey,
    signature : dummy_sig,
  };

  const offchain_bid_data : OffchainBidData = {
    asset_id : assetId,
    bid_amount : bidAmount,
  };

  const fake_permit_bid_param : PermitBidParam = {
    offchain_bid_data : offchain_bid_data,
    permit : fake_permit,
  };

  // Bob preapplies a transfer with the dummy_sig to extract the bytes_to_sign
  const transfer_params = auction.methodsObject.offchain_bid(fake_permit_bid_param)
    .toTransferParams({ amount : 0 });
  const bytes_to_sign = await submitter.estimate.transfer(transfer_params)
    .catch((e) => errors_to_missigned_bytes(e.errors));
  $log.info('bytes_to_sign:', bytes_to_sign);
  // Alice sign the parameter
  const param_sig = await permitSigner.signer.sign(bytes_to_sign).then(s => s.prefixSig);

  const one_step_permit : Permit = {
    signerKey : signerKey,
    signature : param_sig,
  };
  const permit_bid_param : PermitBidParam = {
    offchain_bid_data : offchain_bid_data,
    permit : one_step_permit,
  };
  // This is what a relayer needs to submit the parameter on the signer's behalf
  $log.info('permit package:', permit_bid_param);
  return permit_bid_param;
};

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
  let aliceKey : key;
  let eveKey : key;
  let startTime : Date;
  let endTime : Date;
  let tokenId : BigNumber;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let mintToken : MintNftParam;
  let assetId : nat;
  let queryBalances : QueryBalances;
  let dummy_permit_bid_param : PermitBidParam;

  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenId = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    eveAddress = await tezos.eve.signer.publicKeyHash();
    eveKey = await tezos.eve.signer.publicKey();
    aliceKey = await tezos.alice.signer.publicKey();
    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map,
      },
      owner: bobAddress,
    };
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    assetId = new BigNumber(0);
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

  test('resovled auction with winning offchain bid should only send NFT to winning bidder, not send payment', async () => {

    const bidMutez = new BigNumber(10000000);
    const permit_bid_param = await createPermit(aliceKey, assetId, bidMutez, nftAuctionBob, tezos.alice, tezos.bob);
    const opBid = await nftAuctionBob.methodsObject
      .offchain_bid(permit_bid_param).send({ amount : 0 });
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

  test('offchain bid outbid by normal bid should not return offchain bid', async () => {
    const bidMutez = new BigNumber(10000000);
    const permit_bid_param = await createPermit(aliceKey, assetId, bidMutez, nftAuctionBob, tezos.alice, tezos.bob);
    const opBid = await nftAuctionBob.methodsObject
      .offchain_bid(permit_bid_param).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    const outBidMutez = new BigNumber(200000000);
    const opOutbid = await nftAuctionEve.methods.bid(0).send({ amount : outBidMutez.toNumber(), mutez : true });
    await opOutbid.confirmation();
    $log.info(`Bid placed`);

    const internalOps = opOutbid.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps).toBeUndefined();
  });

  test('offchain bid outbid by another offchain bid should not return first offchain bid', async () => {
    const bidMutez = new BigNumber(10000000);
    const permit_bid_param = await createPermit(aliceKey, assetId, bidMutez, nftAuctionBob, tezos.alice, tezos.bob);
    const opBid = await nftAuctionBob.methodsObject
      .offchain_bid(permit_bid_param).send({ amount : 0 });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    const outBidMutez = new BigNumber(20000000);
    const permit_outbid_param = await createPermit(eveKey, assetId, outBidMutez, nftAuctionBob, tezos.eve, tezos.bob);
    const opOutbid = await nftAuctionBob.methodsObject
      .offchain_bid(permit_outbid_param).send({ amount : 0 });
    await opOutbid.confirmation();
    $log.info(`Bid placed`);

    const internalOps = opOutbid.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps).toBeUndefined();
  });

  test('Offchain bid outbidding normal bid should return that bid', async () => {
    const bidMutez = new BigNumber(10000000);
    const opBid = await nftAuctionEve.methods.bid(0).send({ amount : bidMutez.toNumber(), mutez : true });
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);

    const outBidMutez = new BigNumber(20000000);
    const permit_outbid_param =
      await createPermit(aliceKey, assetId, outBidMutez, nftAuctionBob, tezos.alice, tezos.bob);
    const opOutbid = await nftAuctionBob.methodsObject
      .offchain_bid(permit_outbid_param).send({ amount : 0 });
    await opOutbid.confirmation();
    $log.info(`Bid placed`);

    const internalOps = opOutbid.operationResults[0].metadata.internal_operation_results as InternalOperationResult[];
    expect(internalOps.length).toEqual(1);
    const feeOp = internalOps[0];
    const amountReturned = feeOp.amount;
    const feeDestination = feeOp.destination;
    expect(amountReturned).toEqual(bidMutez.toString());
    expect(feeDestination).toEqual(eveAddress);
  });

  test('Offchain bid by non admin should fail', async () => {
    dummy_permit_bid_param = {
      offchain_bid_data : {
        asset_id : new BigNumber(0),
        bid_amount : new BigNumber(0),
      },
      permit : {
        signerKey : aliceKey,
        signature : dummy_sig,
      },
    };
    const opBid = nftAuctionAlice.methodsObject
      .offchain_bid(dummy_permit_bid_param).send({ amount : 0 });
    expect(opBid).rejects.toHaveProperty('message', 'NOT_AN_ADMIN');
  });

});
