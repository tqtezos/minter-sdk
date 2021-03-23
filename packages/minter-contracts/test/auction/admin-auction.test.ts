import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, nat, bytes, address } from '../../src/type-aliases';
import moment from 'moment'
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
import {Fa2_token, Tokens } from '../../src/auction-interface'

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
  let mintToken : MintNftParam;
  let fa2_token : Fa2_token;
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

    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map
      },
      owner: bobAddress
    };
    const opMint = await nftContract.methods.mint([mintToken]).send();
    const hash = await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');
    
   fa2_token = {
      token_id : tokenId,
      amount : new BigNumber(1)
  }

    auction_tokens = {
      fa2_address : nftContract.address,
      fa2_batch : [fa2_token]
  }
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
            endTime
            ).send({amount : 10} 
          );
    return expect(opAuctionPromise).rejects.toHaveProperty('message', "NOT_AN_ADMIN");
  });  
  
});