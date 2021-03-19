import { $log } from '@tsed/logger';

import { compileAndLoadContract, originateContract, defaultEnv } from './ligo';
import { Contract, address, nat } from './type-aliases';
import { TezosToolkit } from '@taquito/taquito';
import { char2Bytes } from '@taquito/tzip16';
import { TokenMetadata } from './fa2-interface';
import { BigNumber } from 'bignumber.js';

export interface MintNftParam {
    token_metadata: TokenMetadata;
    owner: address;
}

export interface MintFtParam {
    owner: address;
    token_id: nat;
    amount: nat;
}

export interface MintFtPaarm {
    token_medata: TokenMetadata;
    owner: address;
}

export interface SaleTokenParamTez {
    token_for_sale_address: address;
    token_for_sale_token_id: nat;
}

export interface SaleParamTez {
    sale_price: BigNumber;
    sale_token: SaleTokenParamTez;
}

interface AdminStorage {
    admin: string;
    pending_admin?: string;
    paused: boolean;
}

export async function originateNft(
    tz: TezosToolkit,
    admin: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'minter_collection/nft/fa2_multi_nft_asset_simple_admin.mligo',
        'nft_asset_main',
        'fa2_multi_nft_asset.tz'
    );
    const meta_uri = char2Bytes('tezos-storage:content');
    const meta = {
        name: 'example_name',
        description: 'sample_token',
        interfaces: ['TZIP-012','TZIP-016']
    };

    const meta_content = char2Bytes(JSON.stringify(meta,null,2));

    const storage = `(Pair (Pair (Pair (Pair "tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU" True) None)
            (Pair (Pair {} 0) (Pair {} {})))
      { Elt "" 0x${meta_uri} ; Elt "contents" 0x${meta_content} })`;
    return originateContract(tz, code, storage, 'nft');
}

export async function originateNftFaucet(
    tz: TezosToolkit,
    admin: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'minter_collection/nft/fa2_multi_nft_faucet.mligo',
        'nft_faucet_main',
        'fa2_multi_nft_faucet.tz'
    );

    const meta_uri = char2Bytes('tezos-storage:content');
    const meta = {
        name: 'example_name',
        description: 'sample_token',
        interfaces: ['TZIP-012','TZIP-016']
    };

    const meta_content = char2Bytes(JSON.stringify(meta,null,2));

    const storage = `(Pair (Pair (Pair {} 0) (Pair {} {}))
      { Elt "" 0x${meta_uri} ; Elt "contents" 0x${meta_content} })`;
    return originateContract(tz, code, storage, 'nftFaucet');
}

export async function originateFtFaucet(
    tz: TezosToolkit,
    admin: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'minter_collection/ft/fa2_multi_ft_faucet.mligo',
        'ft_faucet_main',
        'fa2_multi_ft_faucet.tz'
    );

    const meta_uri = char2Bytes('tezos-storage:content');
    const meta = {
        name: 'example_name',
        description: 'sample_token',
        interfaces: ['TZIP-012','TZIP-106']
    };

    const meta_content = char2Bytes(JSON.stringify(meta,null,2));

    const storage = `(Pair (Pair (Pair {} {}) (Pair {} {}))
        { Elt "" 0x${meta_uri} ; Elt "contents" 0x${meta_content} })`;

    return originateContract(tz,code,storage,'ftFaucet');
}

export async function originateFixedPriceSale(
    tz: TezosToolkit,
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'fixed_price_sale/fixed_price_sale_market.mligo',
        'fixed_price_sale_main',
        'fixed_price_sale_market.tz',
    );
    const storage = `(Pair None {})`;
    return originateContract(tz, code, storage, 'fixed-price-sale-market');
}

export async function originateFixedPriceTezSale(
    tz: TezosToolkit,
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'fixed_price_sale/fixed_price_sale_market_tez.mligo',
        'fixed_price_sale_tez_main',
        'fixed_price_sale_market_tez.tz',
    );
    const storage = `(Pair None {})`;
    return originateContract(tz, code, storage, 'fixed-price-sale-market-tez');
}

export async function originateFixedPriceAdminSale(
    tz: TezosToolkit,
    adminAddress: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'fixed_price_sale/fixed_price_sale_market.mligo',
        'fixed_price_sale_main',
        'fixed_price_sale_market.tz'
    );
    const storage = `(Pair (Some (Pair (Pair "${adminAddress}" False) None)) {})`;
    return originateContract(tz, code, storage, 'fixed-price-sale-market-with-admin');
}

export async function originateFixedPriceTezAdminSale(
    tz: TezosToolkit,
    adminAddress: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'fixed_price_sale/fixed_price_sale_market_tez.mligo',
        'fixed_price_sale_tez_main',
        'fixed_price_sale_market_tez.tz',
    );
    const storage = `(Pair (Some (Pair (Pair "${adminAddress}" False) None)) {})`
    return originateContract(tz, code, storage, 'fixed-price-sale-market-tez-with-admin');
}

export async function originateEnglishAuctionTez(
    tz: TezosToolkit,
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'english_auction/english_auction_tez.mligo',
        'english_auction_tez_main',
        'english_auction_tez.tz',
    );
    const tzAddress = await tz.signer.publicKeyHash();
    const storage = `(Pair None (Pair 0 (Pair 86400 (Pair 86400 {}))))`;
    return originateContract(tz, code, storage, 'english_auction_tez');
}

export async function originateEnglishAuctionTezAdmin(
    tz: TezosToolkit,
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'english_auction/english_auction_tez.mligo',
        'english_auction_tez_main',
        'english_auction_tez.tz',
    );
    const tzAddress = await tz.signer.publicKeyHash();
    const storage = `(Pair (Some (Pair (Pair "${tzAddress}" False) None)) (Pair 0 (Pair 86400 (Pair 86400 {}))))`;
    return originateContract(tz, code, storage, 'english_auction_tez_admin');
}

export async function originateEditionsNftContract(
    tz: TezosToolkit,
    adminAddress: address
): Promise<Contract> {
    const code = await compileAndLoadContract(
        defaultEnv,
        'minter_collection/editions/fa2_multi_nft_token_editions.mligo',
        'editions_main',
        'fa2_multi_nft_token_editions.tz',
    );
    const tzAddress = await tz.signer.publicKeyHash();
    const meta_uri = char2Bytes('tezos-storage:content');
    const meta = {
        name: 'example_name',
        description: 'sample_token',
        interfaces: ['TZIP-012','TZIP-016']
    };

    const meta_content = char2Bytes(JSON.stringify(meta,null,2));

    const storage = `(Pair (Pair 0 {}) (Pair (Pair (Pair (Pair {"${adminAddress}"} False) {})
    (Pair (Pair {} 0) (Pair {} {}))) { Elt "" 0x${meta_uri} ; Elt "contents" 0x${meta_content} }))`;
    return originateContract(tz, code, storage, 'editions');
}
