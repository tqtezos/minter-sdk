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
} from '../../src/nft-contracts';

import {
  errors_to_missigned_bytes,
  Permit,
  dummy_sig,
} from '../../src/permit';

import {
  addOperator,
} from '../../src/fa2-interface';
import { QueryBalances, queryBalancesWithLambdaView, hasTokens } from '../fa2-balance-inspector';

jest.setTimeout(360000); // 6 minutes

interface PermitBuyParam {
    sale_id : nat;
    permit : Permit;
}

interface ConfirmOrRevokePurchaseParam {
  sale_id : nat;
  purchaser : address;
}

export async function createSale (
  marketplace: Contract,
  price: nat,
  nftAddress: address,
  ftAddress: address,
  nftTokenId : nat,
  ftTokenId : nat,
  tokenAmount: nat,
): Promise<void> {
  $log.info(`Creating sale`);
  const sellOp = await marketplace.methods
    .sell(price, nftAddress, nftTokenId, ftAddress, ftTokenId, tokenAmount)
    .send({ amount: 0 });
  $log.info(`Waiting for ${sellOp.hash} to be confirmed...`);
  const sellOpHash = await sellOp.confirmation().then(() => sellOp.hash);
  $log.info(`Operation injected at hash=${sellOpHash}`);
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
  let ftTokenId: nat;
  let nftTokenId: nat;
  let tokenAmount: nat;
  let tokenMetadata: MichelsonMap<string, bytes>;
  let saleId : nat;
  let salePrice : nat;

  beforeAll(async () => {
    tezos = await bootstrap();
    queryBalances = queryBalancesWithLambdaView(tezos.lambdaView);
    saleId = new BigNumber(0);
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    bobAddress = await tezos.bob.signer.publicKeyHash();
    nftTokenId = new BigNumber(0);
    ftTokenId = new BigNumber(5);
    tokenMetadata = new MichelsonMap();
    salePrice = new BigNumber(20);
    tokenAmount = new BigNumber(1);
  });

  beforeEach(async () => {
    nft = await originateNftFaucet(tezos.bob);
    ft = await originateFtFaucet(tezos.bob);
    marketplace = await originateMarketplace(tezos.bob, bobAddress);
    marketAddress = marketplace.address;

    await mintNftTokens(tezos.bob, [
      {
        token_metadata: {
          token_id: nftTokenId,
          token_info: tokenMetadata,
        },
        owner: bobAddress,
      },
    ], nft);

    $log.info('making marketplace an operator of bob\'s NFT token');
    await addOperator(nft.address, tezos.bob, marketAddress, nftTokenId);

  });

  test('bob makes sale, and alice buys nft with permit submitted by bob on Alices behalf. Bob confirms purchase', async () => {
    await createSale(marketplace, salePrice, nft.address, ft.address, nftTokenId, ftTokenId, tokenAmount);

    const aliceKey = await tezos.alice.signer.publicKey();

    const fake_permit : Permit = {
      signerKey : aliceKey,
      signature : dummy_sig,
    };

    const fake_permit_buy_param : PermitBuyParam = {
      sale_id : saleId,
      permit : fake_permit,
    };

    // Bob preapplies a transfer with the dummy_sig to extract the bytes_to_sign
    const transfer_params = marketplace.methods.offchain_buy([fake_permit_buy_param]).toTransferParams();
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
      permit : one_step_permit,
    };

    // This is what a relayer needs to submit the parameter on the signer's behalf
    $log.info('permit package:', permit_buy_param);


    // Bob submits the permit to the contract
    const permit_op = await marketplace.methods.offchain_buy([permit_buy_param]).send();
    await permit_op.confirmation().then(() => $log.info('permit_op hash:', permit_op.hash));

    const confirm_param : ConfirmOrRevokePurchaseParam = {
      sale_id : saleId,
      purchaser : aliceAddress,
    };

    //Bob confirms the pending purchase
    const confirm_op = await marketplace.methods.confirm_purchases([confirm_param]).send();
    await confirm_op.confirmation();

    const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
      { owner: aliceAddress, token_id: nftTokenId },
      { owner: bobAddress, token_id: nftTokenId },
    ], queryBalances, nft);
    expect(aliceHasATokenAfter).toBe(true);
    expect(bobHasATokenAfter).toBe(false);
  });
  test('bob makes sale, and alice buys nft with permit submitted by bob on Alices behalf. Bob rejects purchase', async () => {

    await createSale(marketplace, salePrice, nft.address, ft.address, nftTokenId, ftTokenId, tokenAmount);

    const aliceKey = await tezos.alice.signer.publicKey();

    const fake_permit : Permit = {
      signerKey : aliceKey,
      signature : dummy_sig,
    };

    const fake_permit_buy_param : PermitBuyParam = {
      sale_id : saleId,
      permit : fake_permit,
    };

    // Bob preapplies a transfer with the dummy_sig to extract the bytes_to_sign
    const transfer_params = marketplace.methods.offchain_buy([fake_permit_buy_param]).toTransferParams();
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
      permit : one_step_permit,
    };

    // This is what a relayer needs to submit the parameter on the signer's behalf
    $log.info('permit package:', permit_buy_param);


    // Bob submits the permit to the contract
    const permit_op = await marketplace.methods.offchain_buy([permit_buy_param]).send();
    await permit_op.confirmation().then(() => $log.info('permit_op hash:', permit_op.hash));

    const revoke_param : ConfirmOrRevokePurchaseParam = {
      sale_id : saleId,
      purchaser : aliceAddress,
    };

    //Bob revokes the pending purchase
    const revoke_op = await marketplace.methods.revoke_purchases([revoke_param]).send();
    await revoke_op.confirmation();

    const [aliceHasATokenAfter, bobHasATokenAfter] = await hasTokens([
      { owner: aliceAddress, token_id: nftTokenId },
      { owner: bobAddress, token_id: nftTokenId },
    ], queryBalances, nft);
    expect(aliceHasATokenAfter).toBe(false); // token stays in escrow
    expect(bobHasATokenAfter).toBe(false);
  });
});
