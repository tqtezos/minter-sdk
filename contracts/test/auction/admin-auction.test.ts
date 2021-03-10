import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, nat, bytes, address } from '../../src/type-aliases';
import {
  originateEnglishAuctionTezAdmin,
  MintNftParam,
  originateNftFaucet
} from '../../src/nft-contracts';
import { TezosToolkit, MichelsonMap } from '@taquito/taquito';

import { TransactionOperation } from '@taquito/taquito/dist/types/operations/transaction-operation';
import {
  OpKind,
  OperationContentsAndResultTransaction,
  OperationResultTransaction
} from '@taquito/rpc';
import {addOperator} from '../../src/fa2-interface'
import {Fa2_tokens, Tokens } from '../../src/auction-interface'

jest.setTimeout(180000); // 3 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
  let nftAuctionAlice : Contract;
  let nftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
  let startTime : Date;
  let endTime : Date;
  let empty_metadata_map: MichelsonMap<string, bytes>;
  let tokenId : BigNumber;
  let token : MintNftParam;
  let fa2_tokens : Fa2_tokens;
  let auction_tokens : Tokens;

  beforeAll(async () => {
    tezos = await bootstrap();
    nftContract = await originateNftFaucet(tezos.bob, bobAddress);
  });

  beforeEach(async() => {
    $log.info('originating nft auction...');
    nftAuction = await originateEnglishAuctionTezAdmin(tezos.bob);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);

    $log.info('minting token')

    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    empty_metadata_map = new MichelsonMap();

    tokenId = new BigNumber(0);

    token = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map
      },
      owner: bobAddress
    };
    const opMint = await nftContract.methods.mint([token]).send();
    const hash = await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');
    
    fa2_tokens = {
        token_id : tokenId,
        amount : new BigNumber(1)
    }

    auction_tokens = {
        fa2_address : nftContract.address,
        fa2_batch : [fa2_tokens]
    }
    
    startTime = new Date();
    startTime.setSeconds(startTime.getSeconds() + 7);
    endTime = new Date(startTime.valueOf());
    endTime.setHours(endTime.getHours() + 1);
    $log.info(`Bob attempts to configure auction`);
    //opening price = 10 tz, percent raise =10, min_raise = 10tz, round_time = 1 hr, extend_time = 5 mins, end_time = start_time + 1hr, 
    const opAuction = await nftAuctionBob.methods.configure(new BigNumber(10000000), new BigNumber(10), new BigNumber(10000000), new BigNumber(3600), new BigNumber(300), [auction_tokens], startTime, endTime).send({amount : 10});
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
  });
  test('configuration not from admin should fail', async() => {
    startTime = new Date();
    startTime.setSeconds(startTime.getSeconds() + 7);
    tokenId = tokenId.plus(1);
    $log.info(`Alice attempts to configure auction, we expect it to fail`);
    //opening price = 10 tz, percent raise =10, min_raise = 10tz, round_time = 1 hr, extend_time = 5 mins, end_time = start_time + 1hr, 
    const opAuctionPromise = nftAuctionAlice.methods.configure(new BigNumber(10000000), new BigNumber(10), new BigNumber(10000000), new BigNumber(3600), new BigNumber(300), [auction_tokens], startTime, endTime).send({amount : 10});
    return expect(opAuctionPromise).rejects.toHaveProperty('message', "NOT_AN_ADMIN");
  });  
  
});