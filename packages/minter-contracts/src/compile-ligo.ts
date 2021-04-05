#!/usr/bin/env node

import { defaultEnv, compileContract, LigoEnv } from './ligo';
import { $log } from '@tsed/logger';
import yargs from 'yargs';

const args = yargs
  .command('$0', 'Compile LIGO to Michelson', y => {
    y.positional('filter', {
      type: 'string',
      describe: 'Optional contract name filter',
    });
  })
  .option('src-path', {
    alias: 's',
    description: 'LIGO source path',
    default: defaultEnv.srcDir,
    type: 'string',
  })
  .option('out-path', {
    alias: 'o',
    description: 'Michelson output path',
    default: defaultEnv.outDir,
    type: 'string',
  })
  .help()
  .argv;

const nameFilter = args._[0] as string | undefined;
const srcPath = args['src-path'];
const outPath = args['out-path'];

type CompileSourceEntry = {
  srcFile: string;
  mainFn: string;
  dstFile: string;
}

// add contracts here
const compileSources: CompileSourceEntry[] = [
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_faucet.mligo',
    mainFn: 'nft_faucet_main',
    dstFile: 'fa2_multi_nft_faucet.tz',
  },
  {
    srcFile:'minter_collection/nft/fa2_multi_nft_asset_simple_admin.mligo',
    mainFn:'nft_asset_main',
    dstFile: 'fa2_multi_nft_asset.tz',
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market.tz',
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez.mligo',
    mainFn: 'fixed_price_sale_tez_main',
    dstFile: 'fixed_price_sale_market_tez.tz',
  },
  {
    srcFile: 'english_auction/english_auction_tez.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez.tz',
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_faucet.mligo',
    mainFn: 'ft_faucet_main',
    dstFile: 'fa2_multi_ft_faucet.tz',
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset.tz',
  },
  {
    srcFile: 'tickets/nft_auction.mligo',
    mainFn: 'ticket_auction_main',
    dstFile: 'ticket_auction.tz',
  },
  {
    srcFile: 'tickets/nft_wallet.mligo',
    mainFn: 'ticket_wallet_main',
    dstFile: 'ticket_wallet.tz',
  },
  {
    srcFile: 'english_auction/english_auction_tez_permit.mligo',
    mainFn: 'english_auction_tez_permit_main',
    dstFile: 'english_auction_tez_permit.tz',
  },
  {
    srcFile: 'minter_collection/editions/fa2_multi_nft_token_editions.mligo',
    mainFn: 'editions_main',
    dstFile: 'fa2_multi_nft_token_editions.tz',
  },
  {
    srcFile: 'swaps/fa2_swap.mligo',
    mainFn: 'swaps_main',
    dstFile: 'fa2_swap.tz',
  },
  {
    srcFile: 'swaps/fa2_whitelisted_swap.mligo',
    mainFn: 'whitelisted_swaps_main',
    dstFile: 'fa2_whitelisted_swap.tz',
  },
  {
    srcFile: 'english_auction/english_auction_fa2.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2.tz',
  },
];

const filterSources = (sources: CompileSourceEntry[]): CompileSourceEntry[] => {
  if (!nameFilter) {
    return sources;
  }

  const regexp = new RegExp(nameFilter as string);
  return sources.filter(({ srcFile, dstFile }) => regexp.test(srcFile) || regexp.test(dstFile));
};

async function main(env = defaultEnv): Promise<void> {
  const sources = filterSources(compileSources);
  if (nameFilter) {
    $log.info(`filter given: filtering contracts matching "${nameFilter}" — ${sources.length} found`);
  }

  $log.info(`compiling ${sources.length} contracts`);

  try {
    await Promise.all(
      filterSources(compileSources)
        .map(async ({ srcFile, mainFn, dstFile }) => {
          $log.info(`compiling ${dstFile}`);
          await compileContract(env, srcFile, mainFn, dstFile);
          $log.info(`compiled ${dstFile}`);
        }),
    );

    process.exit(0);
  } catch (err) {
    $log.error(err);
    process.exit(1);
  }
}

const env = new LigoEnv(process.cwd(), srcPath, outPath);

main(env);
