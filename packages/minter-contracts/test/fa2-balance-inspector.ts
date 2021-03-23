import { address, Contract } from '../src/type-aliases';
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
