import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { MichelsonMap } from '@taquito/taquito';

import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, address, bytes, nat } from '../../src/type-aliases';

import {
  originateNftFaucet,
  originateFtFaucet,
  originateFixedPriceOffchainSale,
  mintNftTokens,
  mintFtTokens,
  createFtToken,
} from '../../src/nft-contracts';

import {
  errors_to_missigned_bytes,
  Permit,
} from '../../src/permit';

import {
  addOperator,
} from '../../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens } from '../fa2-balance-inspector';

jest.setTimeout(360000); // 6 minutes

interface PermitBuyParam {
    sale_id : nat;
    optional_permit? : Permit;
}

describe.each([originateFixedPriceOffchainSale])
('marketplace test', (originateMarketplace) => {
  let tezos: TestTz;
  let nft: Contract;
  let ft: Contract;
  let queryBalances: QueryBalances;
  let marketplace: Contract;
  let marketAddress: address;
  let aliceAddress: address;
  let bobAddress: address;
  let ftTokenId: BigNumber;
  let nftTokenId: BigNumber;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let saleId : nat;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    saleId = new BigNumber(0);
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    nftTokenId = new BigNumber(0);
    ftTokenId = new BigNumber(5);
    tokenMetadata = new MichelsonMap();
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(tezos.bob);
    ft = await originateFtFaucet(tezos.bob);
    marketplace = await originateMarketplace(tezos.bob, bobAddress);
    marketAddress = marketplace.address;
    const tokenAmount = new BigNumber(1);
    await createFtToken(tezos.bob, ft, { token_id : ftTokenId, token_info: tokenMetadata });
    await mintFtTokens(tezos.bob, ft, [
      {
        token_id: ftTokenId,
        owner: bobAddress,
        amount: new BigNumber(1000),
      },
    ]);

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: nftTokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    const [aliceHasNFTTokenBefore, bobHasNFTTokenBefore] = await hasTokens([
      { owner: aliceAddress, token_id: nftTokenId },
      { owner: bobAddress, token_id: nftTokenId },
    ], queryBalances, nft);
    expect(aliceHasNFTTokenBefore).toBe(false);
    expect(bobHasNFTTokenBefore).toBe(true);

    $log.info('making marketplace an operator of Bob\'s FT tokens');
    await addOperator(ft.address, tezos.bob, marketAddress, ftTokenId);

    $log.info('making marketplace an operator of bob\'s NFT token');
    await addOperator(nft.address, tezos.bob, marketAddress, nftTokenId);

    $log.info('starting sale...');

    $log.info(`Creating sale`);
    const sellOp = await marketplace.methods
      .sell(new BigNumber(20), nft.address, nftTokenId, ft.address, ftTokenId, tokenAmount)
      .send({ amount: 0 });
    $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
    const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
    $log.info(`Operation injected at hash=${sellOpHash}`);
  });

  test('bob makes sale, and alice buys nft with permit submitted by bob on Alices behalf', async () => {

    const aliceKey = await tezos.alice.signer.publicKey();
    const dummy_sig = "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm";

    const fake_permit : Permit = {
      signerKey : aliceKey,
      signature : dummy_sig,
    };

    const fake_permit_buy_param : PermitBuyParam = {
      sale_id : saleId,
      optional_permit : fake_permit,
    };

    // Bob preapplies a transfer with the dummy_sig to extract the bytes_to_sign
    const transfer_params = marketplace.methods.permit_buy([fake_permit_buy_param]).toTransferParams();
    const bytes_to_sign = await tezos.bob.estimate.transfer(transfer_params)
      .catch((e) => errors_to_missigned_bytes(e.errors));
    $log.info('bytes_to_sign:', bytes_to_sign);

    // Alice sign the parameter
    const param_sig = await tezos.alice.signer.sign(bytes_to_sign).then(s => s.prefixSig);

    const one_step_permit : Permit = {
      signerKey : aliceKey,
      signature : param_sig,
    };

    const permit_buy_param : PermitBuyParam = {
      sale_id : saleId,
      optional_permit : one_step_permit,
    };

    // This is what a relayer needs to submit the parameter on the signer's behalf
    $log.info('permit package:', permit_buy_param);


    // Bob submits the permit to the contract
    const permit_op = await marketplace.methods.permit_buy([permit_buy_param]).send();
    await permit_op.confirmation().then(() => $log.info('permit_op hash:', permit_op.hash));
  });
});
