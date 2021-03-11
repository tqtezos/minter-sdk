import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, nat, bytes, address } from '../../src/type-aliases';
import {
  MintNftParam,
  originateFtFaucet,
  originateNftFaucet,
  originateEnglishAuctionFA2,
  MintFtParam
} from '../../src/nft-contracts';
import { TezosToolkit, MichelsonMap } from '@taquito/taquito';

import { TransactionOperation } from '@taquito/taquito/dist/types/operations/transaction-operation';
import {
  OpKind,
  OperationContentsAndResultTransaction,
  OperationResultTransaction
} from '@taquito/rpc';
import {addOperator, TokenMetadata} from '../../src/fa2-interface'
import {Fa2_tokens, Tokens } from '../../src/auction-interface'
import { Token } from '@taquito/michelson-encoder/dist/types/tokens/token';

jest.setTimeout(180000); // 3 minutes

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
  let create_ft_param : TokenMetadata;

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
        token_info: empty_metadata_map
      },
      owner: bobAddress
    };
    bid_tokens_bob = {
        token_id: tokenIdBidToken,
        owner: bobAddress,
        amount : new BigNumber(100)
    };
    bid_tokens_alice = {
        token_id: tokenIdBidToken,
        owner: aliceAddress,
        amount : new BigNumber(100)
    };
    create_ft_param = {
        token_id : tokenIdBidToken,
        token_info : empty_metadata_map
    };

    $log.info('originating nft faucet...');
    nftContract = await originateNftFaucet(tezos.bob, bobAddress);

    $log.info('originating ft faucet...');
    ftContract = await originateFtFaucet(tezos.bob, bobAddress);
    
    $log.info('minting nft')
    const opMintNft = await nftContract.methods.mint([nft]).send();
    await opMintNft.confirmation();
    $log.info(`Minted nft. Consumed gas: ${opMintNft.consumedGas}`);

    $log.info('creating fa2 for bids')
    const opCreateFA2 = await ftContract.methods.create_token(tokenIdBidToken, empty_metadata_map).send();
    await opCreateFA2.confirmation();
    $log.info(`Created FA2 Consumed gas: ${opCreateFA2.consumedGas}`);

    $log.info('minting fa2 for bids')
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
    
    const fa2_tokens : Fa2_tokens = {
        token_id : tokenIdNft,
        amount : new BigNumber(1)
    }

    const tokens : Tokens = {
        fa2_address : nftContract.address,
        fa2_batch : [fa2_tokens]
    }
    
    startTime = new Date();
    startTime.setSeconds(startTime.getSeconds() + 7);
    endTime = new Date(startTime.valueOf());
    endTime.setHours(endTime.getHours() + 1);
    $log.info(`Configuring auction`);
    //opening price = 1, percent raise =10, min_raise = 1, round_time = 1 hr, extend_time = 5 mins, end_time = start_time + 1hr, 
    const opAuction = await nftAuctionBob.methods.configure(new BigNumber(1), new BigNumber(10), new BigNumber(1), new BigNumber(3600), new BigNumber(300), [tokens], startTime, endTime).send();
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise_percent', async () => {
    $log.info(`Alice bids 1 token`);
    const opBid = await nftAuctionAlice.methods.bid(0, 1).send();
    await opBid.confirmation();
    $log.info(`Bid placed`);
    
    $log.info(`Alice bids 2 tokens`)
    const opBid2 = await nftAuctionAlice.methods.bid(0, 2).send();
    await opBid2.confirmation();
    $log.info(`Bid placed`);
  });
});
