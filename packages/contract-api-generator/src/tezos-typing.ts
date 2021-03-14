import { ContractMethod, ContractProvider } from '@taquito/taquito';
import { MichelsonMap } from '@taquito/taquito';
import { ContractAbstraction } from '@taquito/taquito';
import { Context } from '@taquito/taquito';
import { TezosToolkit } from '@taquito/taquito';
import { OriginationOperation } from '@taquito/taquito/dist/types/operations/origination-operation';
import { OriginateParams } from '@taquito/taquito/dist/types/operations/types';
import { TezosTypes } from './tezos-types';
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
    contract.methods.bid(TezosTypes.nat.fromNumber(0));
    contract.methods.configure({
        asset: [{
            fa2_address: TezosTypes.address.fromString(`tz123`),
            fa2_batch: [{
                amount: TezosTypes.nat.fromNumber(100),
                token_id: TezosTypes.nat.fromString(`100000000000000`),
            }],
        }],
        start_time: TezosTypes.timestamp.fromDate(new Date()),
        end_time: TezosTypes.timestamp.fromDate(new Date()),
        extend_time: TezosTypes.nat.fromNumber(10),
        min_raise: TezosTypes.mutez.fromNumber(10),
        min_raise_percent: TezosTypes.nat.fromNumber(10),
        opening_price: TezosTypes.mutez.fromNumber(10),
        round_time: TezosTypes.nat.fromNumber(10),
    });

    const Tezos2 = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType2>;

    const originationResult = await Tezos2.contract.originate({
        code: ``,
        storage: {},
    });
    const contract2 = await originationResult.contract(5);
    contract2.methods.set_admin(TezosTypes.address.fromString(`tz123`));
    contract2.methods.create_token({
        token_id: TezosTypes.nat.fromString(`100000000000000`),
        token_info: TezosTypes.map.from<string, ReturnType<typeof TezosTypes['bytes']['fromString']>>([
            { key: `0`, value: TezosTypes.bytes.fromString(`abc`) },
            { key: `1`, value: TezosTypes.bytes.fromString(`def`) },
        ]),
    });

    // contract2.
};