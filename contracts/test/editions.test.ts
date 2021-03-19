import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import {
    BigMapAbstraction,
    TezosToolkit,
    MichelsonMap
} from '@taquito/taquito';


import { char2Bytes } from '@taquito/tzip16';

import { bootstrap, TestTz } from './bootstrap-sandbox';
import { Contract, nat, bytes, address} from '../src/type-aliases';

import {
    originateEditionsNftContract,
    MintNftParam
} from '../src/nft-contracts';
import {
    TokenMetadata
} from '../src/fa2-interface';
//import { QueryBalances, queryBalancesWithLambdaView } from './fa2-balance-inspector';

jest.setTimeout(180000); // 3 minutes

export interface MintEditionParam {
  token_metadata : TokenMetadata;
  number_of_editions : nat;
}

export interface distribute_edition {
  edition_id : nat; 
  receivers : address[];
} 

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftEditions : Contract;
  let nft1 : MintEditionParam;
  let nft2 : MintEditionParam;
  let tokenId : nat;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let bobAddress : address;
  let aliceAddress : address;

  beforeAll(async () => {
    tezos = await bootstrap();
    tokenId = new BigNumber(0);
    empty_metadata_map = new MichelsonMap();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    $log.info('originating editions contract')
    nftEditions = await originateEditionsNftContract(tezos.bob, bobAddress);
    $log.info('editions contract originated')
  });
  
  test('mint 1000 editions of nft1 and 50 of nft2', async() => {
    nft1 = {
      token_metadata : {
        token_id: tokenId,
        token_info: empty_metadata_map
      },
      number_of_editions : new BigNumber(1000)
    };
    
    tokenId = new BigNumber(1001);

    nft2 = {
      token_metadata : {
        token_id: tokenId,
        token_info: empty_metadata_map
      },
      number_of_editions : new BigNumber(50)
    };
    const opMint = await nftEditions.methods.mint_editions([nft1, nft2]).send();
    const hash = await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`) 
  });
  
  test('distribute editions', async() => {
    const distributeEdition0 : distribute_edition = {
      edition_id : new BigNumber(0),
      receivers : [aliceAddress, bobAddress]
    }
    const distributeEdition1 : distribute_edition = {
      edition_id : new BigNumber(1),
      receivers : [aliceAddress, bobAddress]
    }
    const opDistribute = await nftEditions.methods.distribute_editions([distributeEdition0, distributeEdition1]).send();
    const hash = await opDistribute.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opDistribute.consumedGas}`) 
  });
});
