import { Contract, address } from './type-aliases';
import { MichelsonMap, TezosToolkit, UnitValue } from '@taquito/taquito';
import { MichelsonMapKey } from '@taquito/michelson-encoder';
import { $log } from '@tsed/logger';

function makeAllowlist(
  newAllowed: [address],
): MichelsonMap<MichelsonMapKey, unknown> {
  const allowedMap = new MichelsonMap();
  newAllowed.forEach(addr => allowedMap.set(addr, UnitValue));
  return allowedMap;
}

export async function setAllowed(
  tz: TezosToolkit,
  contract: Contract,
  newAllowed: [address],
): Promise<void> {
  $log.info('setting allowlist to ' + newAllowed);
  const op = await contract.methods
    .set_allowed(makeAllowlist(newAllowed))
    .send();
  await op.confirmation(3);
  $log.info(`consumed gas: ${op.consumedGas}`);
}
