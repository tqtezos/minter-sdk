import { ContractMethod, ContractProvider } from '@taquito/taquito';
import { TezosToolkit } from '@taquito/taquito';
import { OriginationOperation } from '@taquito/taquito/dist/types/operations/origination-operation';
import { OriginateParams } from '@taquito/taquito/dist/types/operations/types';

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

