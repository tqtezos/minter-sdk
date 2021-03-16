import { address, Contract } from '../src/type-aliases';
import { BalanceOfRequest, BalanceOfResponse } from '../src/fa2-interface';

const queryBalances = async (
    fa2: Contract,
    requests: BalanceOfRequest[],
    lambdaView?: address
): Promise<BalanceOfResponse[]> =>
    await fa2.views.balance_of(requests).read(lambdaView);

export type QueryBalances = (
    fa2: Contract,
    requests: BalanceOfRequest[]
) => Promise<BalanceOfResponse[]>;

export const queryBalancesWithLambdaView = (
    lambdaView?: address
): QueryBalances => (fa2: Contract, requests: BalanceOfRequest[]) =>
    queryBalances(fa2, requests, lambdaView);
