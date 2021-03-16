#!/usr/bin/env node

import * as fs from 'fs';
import { defaultEnv, LigoEnv, compileContract} from './ligo';
import { $log } from '@tsed/logger';

async function main(): Promise<void> {
  try {
    const env = defaultEnv;

    await compileNftFaucetContract(env);
    await compileNftContract(env);
    await compileFixedPriceSaleMarketPlaceContract(env);
    await compileFixedPriceSaleTezMarketPlaceContract(env);
    await compileEnglishAuctionTezContract(env);
    await compileFtFaucetContract(env);
    await compileFtContract(env);
    //compile english_auction_tez_permit.mligo manually from contracts directory like: 
    //docker run --rm -v $PWD:$PWD -w $PWD ligolang/ligo:next compile-contract ligo/src/english_auction/english_auction_tez_permit.mligo english_auction_tez_permit_main --output=bin/english_auction_tez_permit.tz
    //await compileEnglishAuctionTezPermitContract(env)
    // add other contracts here

    process.exit(0);
  } catch (err) {
    $log.error(err);
    process.exit(1);
  }
}

async function compileNftFaucetContract(env: LigoEnv): Promise<void> {
  $log.info('compiling NFT faucet contract');
  await await compileContract(
    env,
    'minter_collection/fa2_multi_nft_faucet.mligo',
    'nft_faucet_main',
    'fa2_multi_nft_faucet.tz'
  );
  $log.info('compiled NFT faucet contract');
}

async function compileNftContract(env: LigoEnv): Promise<void> {
  $log.info('compiling NFT contract');
  await await compileContract(
    env,
    'minter_collection/fa2_multi_nft_asset.mligo',
    'nft_asset_main',
    'fa2_multi_nft_asset.tz'
  );
  $log.info('compiled NFT contract');
}


async function compileFtFaucetContract(env: LigoEnv): Promise<void> {
  $log.info('compiling FT faucet contract');
  await await compileContract(
    env,
    'minter_collection/fa2_multi_ft_faucet.mligo',
    'ft_faucet_main',
    'fa2_multi_ft_faucet.tz'
  );
  $log.info('compiled FT faucet contract');
}

async function compileFtContract(env: LigoEnv): Promise<void> {
  $log.info('compiling FT contract');
  await await compileContract(
    env,
    'minter_collection/fa2_multi_ft_asset.mligo',
    'multi_ft_asset_main',
    'fa2_multi_ft_asset.tz'
  );
  $log.info('compiled NFT contract');
}

async function compileFixedPriceSaleMarketPlaceContract(env: LigoEnv): Promise<void> {
    $log.info('compiling fixed price sale marketplace contract');

    await await compileContract(
        env,
        'fixed_price_sale/fixed_price_sale_market.mligo',
        'fixed_price_sale_main',
        'fixed_price_sale_market.tz'
    );
    $log.info('compiled fixed price sale marketplace contract');
}

async function compileFixedPriceSaleTezMarketPlaceContract(env: LigoEnv): Promise<void> {
    $log.info('compiling fixed price sale (sold in tez) marketplace contract');

    await compileContract(
        env,
        'fixed_price_sale/fixed_price_sale_market_tez.mligo',
        'fixed_price_sale_tez_main',
        'fixed_price_sale_market_tez.tz'
    );
    $log.info('compiled fixed price sale (sold in tez) marketplace contract');
}

async function compileEnglishAuctionTezContract(env: LigoEnv): Promise<void> {
  $log.info('compiling english auction tez contract');

  await compileContract(
      env,
      'english_auction/english_auction_tez.mligo',
      'english_auction_tez_main',
      'english_auction_tez.tz'
  );
  $log.info('compiled english auction tez contract');
}

async function compileEnglishAuctionTezPermitContract(env: LigoEnv): Promise<void> {
  $log.info('compiling english auction tez permit contract');

  await compileContract(
      env,
      'english_auction/english_auction_tez_permit.mligo',
      'english_auction_tez_permit_main',
      'english_auction_tez_permit.tz'
  );
  $log.info('compiled english auction tez permit contract');
}

main();
