import { TransactionOperationParameter } from '@taquito/rpc';
import { ContractMethod, ContractProvider } from '@taquito/taquito';
import { TezosToolkit } from '@taquito/taquito';
import { OriginationOperation } from '@taquito/taquito/dist/types/operations/origination-operation';
import { TransactionOperation } from '@taquito/taquito/dist/types/operations/transaction-operation';
import { OriginateParams } from '@taquito/taquito/dist/types/operations/types';
import { address, mutez, nat, tez } from './type-aliases';

type ContractTypeBase = {
    storage: unknown;
    methods: unknown;
};
type TypedContractOf<T extends ContractTypeBase> = {
    storage: () => Promise<T['storage']>;
    methods: { [M in keyof T['methods']]:
        T['methods'][M] extends (...args: infer A) => Promise<void>
        ? (...args: A) => TypedContractMethod// ContractMethod<ContractProvider>
        : never
    };
};
type TypedContractProviderOf<T extends ContractTypeBase> = Omit<ContractProvider, 'at' | 'originate' | 'storage'> & {
    at: (address: string) => Promise<TypedContractOf<T>>;
    originate(contract: OriginateParams): Promise<Omit<OriginationOperation, 'contract'> & {
        contract: (confirmations?: number, interval?: number, timeout?: number) => Promise<TypedContractOf<T>>;
    }>;
};

type TypedSentParams = ({
    mutez?: false;
    amount: tez;
} | {
    mutez: true;
    amount: mutez;
}) & {
    fee?: mutez;
    gasLimit?: nat;
    source?: address;
    storageLimit?: nat;
};

type TypedTransferParams = TypedSentParams & {
    to: address;
    parameter?: TransactionOperationParameter;
};


type TypedContractMethod = {
    send(params?: Partial<TypedSentParams>): Promise<TransactionOperation>;
    toTransferParams({ fee, gasLimit, storageLimit, source, amount, mutez }?: Partial<TypedSentParams>): TypedTransferParams;
};

export type TezosToolkitTyped<T extends ContractTypeBase> = Omit<TezosToolkit, 'contract'> & {
    contract: TypedContractProviderOf<T>;
};

