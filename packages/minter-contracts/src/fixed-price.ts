import { $log } from '@tsed/logger';
import { Contract, TezosToolkit, MichelsonMap } from '@taquito/taquito';
import { address, nat } from './type-aliases';
import {
  mintFtTokens,
  createFtToken,
} from '../src/nft-contracts';
import {
  addOperator,
} from '../src/fa2-interface';

export async function createPurchaseToken (
  marketAddress : address,
  ft : Contract,
  ftAddress: address,
  ftTokenId : nat,
  purchaser : TezosToolkit,
  purchaserAddress : address,
  tokenMetadata : MichelsonMap<string, string>,
  mintAmount : nat,
) : Promise<void> {
  await createFtToken(purchaser, ft, { token_id : ftTokenId, token_info: tokenMetadata });
  await mintFtTokens(purchaser, ft, [
    {

      token_id: ftTokenId,
      owner: purchaserAddress,
      amount: mintAmount,
    },
  ]);
  $log.info('making marketplace an operator of purchaser\'s FT tokens');
  await addOperator(ftAddress, purchaser, marketAddress, ftTokenId);
}
