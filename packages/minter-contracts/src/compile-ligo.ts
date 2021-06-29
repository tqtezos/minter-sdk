#!/usr/bin/env node

import { defaultEnv, compileContract, LigoEnv, compileLigoExpression } from './ligo';
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
  contract: boolean;
}

// add contracts here
const compileSources: CompileSourceEntry[] = [
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_faucet.mligo',
    mainFn: 'nft_faucet_main',
    dstFile: 'fa2_multi_nft_faucet.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_asset_no_admin.mligo',
    mainFn: 'nft_asset_main',
    dstFile: 'fa2_multi_nft_asset_no_admin.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_asset_simple_admin.mligo',
    mainFn: 'nft_asset_main',
    dstFile: 'fa2_multi_nft_asset.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_asset_multi_admin.mligo',
    mainFn: 'nft_asset_main',
    dstFile: 'fa2_multi_nft_asset_multi_admin.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/nft/fa2_multi_nft_asset_non_pausable_simple_admin.mligo',
    mainFn: 'nft_asset_main',
    dstFile: 'fa2_multi_nft_asset_non_pausable_simple_admin.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_allowlisted_token.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_allowlisted_token.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_faucet.mligo',
    mainFn: 'ft_faucet_main',
    dstFile: 'fa2_multi_ft_faucet.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset_no_admin.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset_no_admin.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset_simple_admin.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset_multi_admin.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset_multi_admin.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset_non_pausable_simple_admin.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset_non_pausable_simple_admin.tz',
    contract: true,
  },
  {
    srcFile: 'tickets/nft_auction.mligo',
    mainFn: 'ticket_auction_main',
    dstFile: 'ticket_auction.tz',
    contract: true,
  },
  {
    srcFile: 'tickets/nft_wallet.mligo',
    mainFn: 'ticket_wallet_main',
    dstFile: 'ticket_wallet.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_permit.mligo',
    mainFn: 'english_auction_tez_permit_main',
    dstFile: 'english_auction_tez_permit.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/editions/fa2_multi_nft_token_editions.mligo',
    mainFn: 'editions_main',
    dstFile: 'fa2_multi_nft_token_editions.tz',
    contract: true,
  },
  {
    srcFile: 'swaps/fa2_swap.mligo',
    mainFn: 'swaps_main',
    dstFile: 'fa2_swap.tz',
    contract: true,
  },
  {
    srcFile: 'swaps/fa2_allowlisted_swap.mligo',
    mainFn: 'allowlisted_swaps_main',
    dstFile: 'fa2_allowlisted_swap.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/editions/views.mligo',
    mainFn: 'token_metadata',
    dstFile: 'editions_token_metadata_view.tz',
    contract: false,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_allowlisted.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_allowlisted.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_permit_allowlisted.mligo',
    mainFn: 'english_auction_tez_permit_main',
    dstFile: 'english_auction_tez_permit_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_allowlisted_token.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_allowlisted_token.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_permit_allowlisted_token.mligo',
    mainFn: 'english_auction_tez_permit_main',
    dstFile: 'english_auction_tez_permit_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_fixed_fee.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_fixed_fee.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_fixed_fee.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_fixed_fee.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_fixed_fee_allowlisted.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_fixed_fee_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_fixed_fee_allowlisted.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_fixed_fee_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_fixed_fee_allowlisted_token.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_fixed_fee_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_fixed_fee_allowlisted_token.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_fixed_fee_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_tez_fixed_fee.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_fixed_fee.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_tez_fixed_fee_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_fixed_fee_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez.mligo',
    mainFn: 'getActiveSales',
    dstFile: 'fixed_price_tez_sales_view.tz',
    contract: false,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market.mligo',
    mainFn: 'getActiveSales',
    dstFile: 'fixed_price_fa2_sales_view.tz',
    contract: false,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fa2_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_offchain.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_tez_offchain.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_cancel_only_admin.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_cancel_only_admin.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_tez_cancel_only_admin.mligo',
    mainFn: 'english_auction_tez_main',
    dstFile: 'english_auction_tez_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'english_auction/english_auction_fa2_cancel_only_admin.mligo',
    mainFn: 'english_auction_fa2_main',
    dstFile: 'english_auction_fa2_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee_cancel_only_admin.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_fixed_fee_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee_cancel_only_admin.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez_fixed_fee_cancel_only_admin.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_tez_fixed_fee_allowlisted.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee_allowlisted.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_fixed_fee_allowlisted.tz',
    contract: true,
  }, {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee_allowlisted_token.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_tez_fixed_fee_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee_allowlisted_token.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_fixed_fee_allowlisted_token.tz',
    contract: true,
  },
  {
    srcFile: 'minter_collection/ft/fa2_multi_ft_asset_limited_simple_admin.mligo',
    mainFn: 'multi_ft_asset_main',
    dstFile: 'fa2_multi_ft_asset_limited.tz',
    contract : true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_fixed_fee_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_fixed_fee_offchain.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_fixed_fee_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_tez_fixed_fee_offchain.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_per_sale_fee.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_tez_per_sale_fee.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_per_sale_fee.mligo',
    mainFn: 'fixed_price_sale_main',
    dstFile: 'fixed_price_sale_market_per_sale_fee.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_tez_per_sale_fee_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_tez_per_sale_fee_offchain.tz',
    contract: true,
  },
  {
    srcFile: 'fixed_price_sale/fixed_price_sale_market_per_sale_fee_offchain.mligo',
    mainFn: 'fixed_price_sale_permit_main',
    dstFile: 'fixed_price_sale_market_per_sale_fee_offchain.tz',
    contract: true,
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
        .map(async ({ srcFile, mainFn, dstFile, contract }) => {
          $log.info(`compiling ${dstFile}`);
          if (contract) {
            await compileContract(env, srcFile, mainFn, dstFile);
          }
          else {
            await compileLigoExpression(env, srcFile, mainFn, dstFile);
          }
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
