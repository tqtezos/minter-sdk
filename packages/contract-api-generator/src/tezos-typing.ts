import { ContractMethod, ContractProvider } from '@taquito/taquito';
import { MichelsonMap } from '@taquito/taquito';
import { ContractAbstraction } from '@taquito/taquito';
import { Context } from '@taquito/taquito';
import { TezosToolkit } from '@taquito/taquito';
import { OriginationOperation } from '@taquito/taquito/dist/types/operations/origination-operation';
import { OriginateParams } from '@taquito/taquito/dist/types/operations/types';
import { tas, TezosTypes } from './tezos-types';
import { TestContractType } from './test-contract-type';
import { TestContractType2 } from './test-contract-type-2';


type ContractTypeBase = {
    storage: unknown;
    methods: unknown;
};
type TypedContractOf<T extends ContractTypeBase> = {
    methods: { [M in keyof T['methods']]:
        T['methods'][M] extends (...args: infer A) => Promise<void>
        ? (...args: A) => ContractMethod<ContractProvider>
        : never };
};
type TypedContractProviderOf<T extends ContractTypeBase> = Omit<ContractProvider, 'at' | 'originate'> & {
    at: (address: string) => Promise<TypedContractOf<T>>;
    originate(contract: OriginateParams): Promise<Omit<OriginationOperation, 'contract'> & {
        contract: (confirmations?: number, interval?: number, timeout?: number) => Promise<TypedContractOf<T>>;
    }>;
};

export type TezosToolkitTyped<T extends ContractTypeBase> = Omit<TezosToolkit, 'contract'> & {
    contract: TypedContractProviderOf<T>;
};



const test = async () => {

    // type typedContract = TypedContractOf<TestContractType>;

    const Tezos = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType>;


    const contract = await Tezos.contract.at(``);
    contract.methods.bid(tas.nat(0));
    contract.methods.configure({
        asset: [{
            fa2_address: tas.address(`tz123`),
            fa2_batch: [{
                amount: tas.nat(100),
                token_id: tas.nat(`100000000000000`),
            }],
        }],
        start_time: tas.timestamp(new Date()),
        end_time: tas.timestamp(`2020-01-01`),
        extend_time: tas.nat(10),
        min_raise: tas.mutez(10),
        min_raise_percent: tas.nat(10),
        opening_price: tas.mutez(10),
        round_time: tas.nat(10),
    });

    const Tezos2 = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType2>;

    const originationResult = await Tezos2.contract.originate({
        code: ``,
        storage: {},
    });
    const contract2 = await originationResult.contract(5);
    contract2.methods.set_admin(tas.address(`tz123`));
    contract2.methods.create_token({
        token_id: tas.nat(`100000000000000`),
        token_info: tas.map([
            { key: `0`, value: tas.bytes(`abc`) },
            { key: `1`, value: tas.bytes(`def`) },
        ]),
    });
    contract2.methods.create_token({
        token_id: tas.nat(`100000000000000`),
        token_info: tas.map({
            0: tas.bytes(`abc`),
            1: tas.bytes(`def`),
        }),
    });

    // contract2.
};