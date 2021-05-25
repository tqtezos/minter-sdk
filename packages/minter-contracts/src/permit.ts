import { ContractAbstraction, ContractProvider, TezosToolkit } from '@taquito/taquito';
import { buf2hex, hex2buf } from '@taquito/utils';
import { $log } from '@tsed/logger';
import { key, signature } from '../src/type-aliases';
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import * as blake from 'blakejs';

export interface Permit {
  signerKey : key;
  signature : signature;
}

export const errors_to_missigned_bytes = (errors : any) => {
  const errors_with = errors.map( (x : any) => x.with).filter((x : any) => x !== undefined);
  if (errors_with.length != 1){
    throw ['errors_to_missigned_bytes: expected one error to fail "with" michelson, but found:', errors_with];
  } else {
    const error_with = errors_with[0];
    if (error_with.prim !== 'Pair'){
      throw ['errors_to_missigned_bytes: expected a "Pair", but found:', error_with.prim];
    } else {
      const error_with_args = error_with.args;
      if (error_with_args.length !== 2){
        throw ['errors_to_missigned_bytes: expected two arguments to "Pair", but found:', error_with_args];
      } else {
        if (error_with_args[0].string !== 'MISSIGNED'){
          throw ['errors_to_missigned_bytes: expected a "missigned" annotation, but found:', error_with_args[0]];
        } else {
          if (typeof error_with_args[1].bytes !== 'string'){
            throw ['errors_to_missigned_bytes: expected bytes, but found:', error_with_args[1].bytes];
          } else {
            return error_with_args[1].bytes;
          }
        }
      }
    }
  }
};

export async function permitParamHash(tz: TezosToolkit,
  contract: ContractAbstraction<ContractProvider>,
  entrypoint: string,
  parameter: any): Promise<string> {
  const contract_entrypoint : any = contract.methods[entrypoint];
  const wrapped_param = contract_entrypoint(parameter).toTransferParams().parameter.value;
  const wrapped_param_type = contract.entrypoints.entrypoints[entrypoint];
  const raw_packed = await tz.rpc.packData({
    data: wrapped_param,
    type: wrapped_param_type,
  }).catch(e => console.error('error:', e));
  let packed_param;
  if (raw_packed) {
    packed_param = raw_packed.packed;
  } else {
    throw `packing ${parameter} failed`;
  };

  return buf2hex(blake.blake2b(hex2buf(packed_param), null, 32));
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const permit_examples = async (tz: TezosToolkit) => {
  $log.info('inside: permit_examples');

  // Get the contract
  const permit_address = 'KT1TDmx9JMdYqpFnD3XBrtZbN6nud1GsQnzU';
  const permit_contract = await tz.contract.at(permit_address);

  // Get the signer's public key and a dummy signature to trigger the error
  const signer_key = await tz.signer.publicKey().catch(e => console.error(e));
  const dummy_sig = "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm";

  // Get the Blake2B hash of the packed parameter
  const param_hash = await permitParamHash(tz, permit_contract, 'wrapped', 42);
  $log.info('permitParamHash:', param_hash);

  const expected_param_hash = "0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c";
  if(param_hash !== expected_param_hash) {
    throw `unexpected param_hash: {param_hash},\n
    while {expected_param_hash} was expected`;
  }

  // Preapply a transfer with the dummy_sig to extract the bytes_to_sign
  const transfer_params = permit_contract.methods.permit(signer_key, dummy_sig, param_hash).toTransferParams();
  const bytes_to_sign = await tz.estimate.transfer(transfer_params).catch((e) => errors_to_missigned_bytes(e.errors));
  $log.info('bytes_to_sign:', bytes_to_sign);

  // Sign the parameter
  const param_sig = await tz.signer.sign(bytes_to_sign).then(s => s.prefixSig);

  // This is what a relayer needs to submit the parameter on the signer's behalf
  $log.info('permit package:', [signer_key, param_sig, param_hash]);

  // Submit the permit to the contract
  const permit_op = await permit_contract.methods.permit(signer_key, param_sig, param_hash).send();
  await permit_op.confirmation().then(() => $log.info('permit_op hash:', permit_op.hash));

  $log.info('ending: permit_examples');
};
