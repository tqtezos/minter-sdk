import { $log } from '@tsed/logger';
import { originateContract } from './ligo';
import { Contract, address, nat } from './type-aliases';
import { TezosToolkit } from '@taquito/taquito';
import { char2Bytes } from '@taquito/tzip16';
import { TokenMetadata } from './fa2-interface';
import { BigNumber } from 'bignumber.js';
import {
  EditionsTokenMetadataViewCode,
  Fa2MultiNftAssetCode,
  Fa2MultiNftAssetNoAdminCode,
  Fa2MultiFtAssetNoAdminCode,
  FixedPriceSaleMarketCode,
  FixedPriceSaleMarketTezCode,
  FixedPriceSaleMarketAllowlistedCode,
  FixedPriceSaleMarketTezAllowlistedCode,
  FixedPriceSaleMarketTezOffchainCode,
  FixedPriceSaleMarketOffchainCode,
  EnglishAuctionTezCode,
  Fa2MultiNftTokenEditionsCode,
  EnglishAuctionFa2Code,
  EnglishAuctionTezPermitCode,
  EnglishAuctionTezFixedFeeCode,
  EnglishAuctionFa2FixedFeeCode,
} from '../bin-ts';

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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
interface AdminStorage {
    admin: string;
    pending_admin?: string;
    paused: boolean;
}

export async function mintNftTokens(
  tz: TezosToolkit,
  tokens: MintNftParam[],
  nftContract: Contract,
): Promise<void> {
  $log.info('minting...');
  const op = await nftContract.methods.mint(tokens).send();
  await op.confirmation(3);
  $log.info(`Minted non-fungible tokens. Consumed gas: ${op.consumedGas}`);
}

export async function mintFtTokens(
  tz: TezosToolkit,
  ftContract: Contract,
  tokens: MintFtParam[],
): Promise<void> {
  $log.info('minting...');
  const op = await ftContract.methods.mint_tokens(tokens).send();
  await op.confirmation(3);
  $log.info(`Minted fungible tokens. Consumed gas: ${op.consumedGas}`);
}

export async function createFtToken(
  tz: TezosToolkit,
  ftContract: Contract,
  token_metadata: TokenMetadata,
): Promise<void> {
  $log.info('minting...');
  const op = await ftContract.methods.create_token(token_metadata.token_id, token_metadata.token_info).send();
  await op.confirmation(3);
  $log.info(`Created fungible token. Consumed gas: ${op.consumedGas}`);
}

const meta_uri = char2Bytes('tezos-storage:content');

const sample_metadata = {
  name: 'example_name',
  description: 'sample_token',
  interfaces: ['TZIP-012', 'TZIP-016'],
};

export async function originateNft(
  tz: TezosToolkit,
  admin: address,
): Promise<Contract> {
  const meta_content = char2Bytes(JSON.stringify(sample_metadata, null, 2));

  const storage = `(Pair (Pair (Pair (Pair ${admin} True) None) (Pair (Pair {} 0) (Pair {} {}))) { Elt "" 0x${meta_uri} ; Elt "content" 0x${meta_content} })`;
  return originateContract(tz, Fa2MultiNftAssetCode.code, storage, 'nft');
}

export async function originateNftFaucet(
  tz: TezosToolkit,
): Promise<Contract> {
  const meta_content = char2Bytes(JSON.stringify(sample_metadata, null, 2));

  const storage = `(Pair (Pair Unit (Pair (Pair {} 0) (Pair {} {})))
      { Elt "" 0x${meta_uri} ; Elt "content" 0x${meta_content} })`;
  return originateContract(tz, Fa2MultiNftAssetNoAdminCode.code, storage, 'nftFaucet');
}

export async function originateFtFaucet(
  tz: TezosToolkit,
): Promise<Contract> {
  const meta_content = char2Bytes(JSON.stringify(sample_metadata, null, 2));

  const storage = `(Pair (Pair Unit (Pair (Pair {} {}) (Pair {} {})))
        { Elt "" 0x${meta_uri} ; Elt "content" 0x${meta_content} })`;

  return originateContract(tz, Fa2MultiFtAssetNoAdminCode.code, storage, 'ftFaucet');
}

export async function originateFixedPriceSale(
  tz: TezosToolkit,
): Promise<Contract> {
  const storage = `(Pair None (Pair {} 0))`;
  return originateContract(tz, FixedPriceSaleMarketCode.code, storage, 'fixed-price-sale-market');
}

export async function originateFixedPriceTezSale(
  tz: TezosToolkit,
): Promise<Contract> {
  const storage = `(Pair None (Pair {} 0))`;
  return originateContract(tz, FixedPriceSaleMarketTezCode.code, storage, 'fixed-price-sale-market-tez');
}

export async function originateFixedPriceAdminSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0))`;
  return originateContract(tz, FixedPriceSaleMarketCode.code, storage, 'fixed-price-sale-market-with-admin');
}

export async function originateFixedPriceTezAdminSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0))`;
  return originateContract(tz, FixedPriceSaleMarketTezCode.code, storage, 'fixed-price-sale-market-tez-with-admin');
}

export async function originateFixedPriceAllowlistedSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair {} (Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0)))`;
  return originateContract(tz, FixedPriceSaleMarketAllowlistedCode.code, storage, 'fixed-price-sale-market-allowlisted');
}

export async function originateFixedPriceTezAllowlistedSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair {} (Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0)))`;
  return originateContract(tz, FixedPriceSaleMarketTezAllowlistedCode.code, storage, 'fixed-price-sale-market-tez-allowlisted');
}

export async function originateEnglishAuctionTez(
  tz: TezosToolkit,
): Promise<Contract> {
  const storage = `(Pair None (Pair 0 (Pair 86400 (Pair 86400 {}))))`;
  return originateContract(tz, EnglishAuctionTezCode.code, storage, 'english_auction_tez');
}

export async function originateEnglishAuctionTezAdmin(
  tz: TezosToolkit,
): Promise<Contract> {
  const tzAddress = await tz.signer.publicKeyHash();
  const storage = `(Pair (Some (Pair (Pair "${tzAddress}" False) None)) (Pair 0 (Pair 86400 (Pair 86400 {}))))`;
  return originateContract(tz, EnglishAuctionTezCode.code, storage, 'english_auction_tez_admin');
}

export async function originateEditionsNftContract(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const editions_metadata = {
    name: 'editions',
    description: 'editions',
    interfaces: ['TZIP-012', 'TZIP-016'],
    views : [{
      name: 'token_metadata',
      description: 'Get the metadata for the tokens minted using this contract',
      pure: true,
      implementations: [
        { michelsonStorageView :
           {
             parameter : {
               prim: 'nat',
             },
             returnType : {
               prim : "pair",
               args : [
                 { prim: "nat", annots: ["%token_id"] },
                 { prim :"map", args :[{ prim:"string" }, { prim:"bytes" }], annots:["%token_info"] },
               ],
             },
             code : EditionsTokenMetadataViewCode.code,
           },
        },
      ],
    }],
  };

  const editions_meta_encoded = char2Bytes(JSON.stringify(editions_metadata, null, 2));
  const max_editions_per_run = 10000;

  const storage = `(Pair (Pair {} ${max_editions_per_run}) (Pair 0 (Pair (Pair (Pair (Pair "${adminAddress}" False) None) (Pair {} {})) { Elt "" 0x${meta_uri} ; Elt "content" 0x${editions_meta_encoded} }))) `;
  return originateContract(tz, Fa2MultiNftTokenEditionsCode.code, storage, 'editions');
}

export async function originateEnglishAuctionFA2(
  tz: TezosToolkit,
  fa2_address : address,
  token_id : nat,
): Promise<Contract> {
  const storage = `(Pair None (Pair 0 (Pair 86400 (Pair 86400 (Pair (Pair "${fa2_address}" ${token_id}){})))))`;
  return originateContract(tz, EnglishAuctionFa2Code.code, storage, 'english_auction_fa2');
}

export async function originateEnglishAuctionFA2Admin(
  tz: TezosToolkit,
  adminAddress: address,
  fa2_address : address,
  token_id : nat,
): Promise<Contract> {
  const storage = `(Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair 0 (Pair 86400 (Pair 86400 (Pair (Pair "${fa2_address}" ${token_id}){})))))`;
  return originateContract(tz, EnglishAuctionFa2Code.code, storage, 'english_auction_fa2');
}

export async function originateEnglishAuctionTezPermit(
  tz: TezosToolkit,
  adminAddress : address,
): Promise<Contract> {
  const storage = `(Pair (Pair (Pair "${adminAddress}" False) None) (Pair 0 (Pair 86400 (Pair 86400 (Pair {} 0)))))`;
  return originateContract(tz, EnglishAuctionTezPermitCode.code, storage, 'english_auction_tez_permit');
}

export async function originateEnglishAuctionTezFixedFee(
  tz: TezosToolkit,
  feeAddress : address,
): Promise<Contract> {
  const storage = `(Pair None (Pair 0 (Pair 86400 (Pair 86400 (Pair {} (Pair "${feeAddress}" 10))))))`;
  return originateContract(tz, EnglishAuctionTezFixedFeeCode.code, storage, 'english_auction_tez');
}

export async function originateEnglishAuctionFA2FixedFee(
  tz: TezosToolkit,
  feeAddress : address,
  fa2_address : address,
  token_id : nat,
): Promise<Contract> {
  const storage = `(Pair None (Pair 0 (Pair 86400 (Pair 86400 (Pair (Pair "${fa2_address}" ${token_id}) (Pair {} (Pair "${feeAddress}" 10)))))))`;
  return originateContract(tz, EnglishAuctionFa2FixedFeeCode.code, storage, 'english_auction_fa2');
}

export async function originateFixedPriceOffchainSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair (Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0)) 0)`;
  return originateContract(tz, FixedPriceSaleMarketOffchainCode.code, storage, 'fixed-price-sale-market-offchain');
}

export async function originateFixedPriceTezOffchainSale(
  tz: TezosToolkit,
  adminAddress: address,
): Promise<Contract> {
  const storage = `(Pair (Pair (Some (Pair (Pair "${adminAddress}" False) None)) (Pair {} 0)) 0)`;
  return originateContract(tz, FixedPriceSaleMarketTezOffchainCode.code, storage, 'fixed-price-sale-market-tez-offchain');
}