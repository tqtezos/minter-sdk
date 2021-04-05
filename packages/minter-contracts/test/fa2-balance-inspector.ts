import { address, Contract, nat } from '../src/type-aliases';
import { BalanceOfRequest, BalanceOfResponse } from '../src/fa2-interface';

export const queryBalancesWithLambdaView = (
  lambdaView: address | undefined,
) => async (
  fa2: Contract,
  requests: BalanceOfRequest[],
): Promise<BalanceOfResponse[]> =>
  await fa2.views.balance_of(requests).read(lambdaView);

export type QueryBalances = (
    fa2: Contract,
    requests: BalanceOfRequest[]
) => Promise<BalanceOfResponse[]>;

export async function hasTokens(requests: BalanceOfRequest[],
  queryBalances : QueryBalances,
  nft : Contract,
): Promise<boolean[]> {
  const responses = await queryBalances(nft, requests);
  const results = responses.map(r => {
    if (r.balance.eq(1)) return true;
    else if (r.balance.eq(0)) return false;
    else throw new Error(`Invalid NFT balance ${r.balance}`);
  });
  return results;
}

export async function getBalances(requests: BalanceOfRequest[],
  queryBalances : QueryBalances,
  nft : Contract,
): Promise<nat[]> {
  const responses = await queryBalances(nft, requests);
  const results = responses.map(r => {
    return r.balance;
  });
  return results;
}
