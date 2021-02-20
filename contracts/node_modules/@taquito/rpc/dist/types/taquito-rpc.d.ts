import { HttpBackend } from '@taquito/http-utils';
import BigNumber from 'bignumber.js';
import { BakingRightsQueryArguments, BakingRightsResponse, BalanceResponse, BallotListResponse, BallotsResponse, BigMapGetResponse, BigMapKey, BigMapResponse, BlockHeaderResponse, BlockMetadata, BlockResponse, ConstantsResponse, ContractResponse, CurrentProposalResponse, CurrentQuorumResponse, DelegateResponse, DelegatesResponse, EndorsingRightsQueryArguments, EndorsingRightsResponse, EntrypointsResponse, ForgeOperationsParams, ManagerKeyResponse, OperationHash, PackDataParams, PeriodKindResponse, PreapplyParams, PreapplyResponse, ProposalsResponse, RPCRunOperationParam, ScriptResponse, StorageResponse, VotesListingsResponse } from './types';
export * from './types';
export { OpKind } from './opkind';
interface RPCOptions {
    block: string;
}
/***
 * @description RpcClient allows interaction with Tezos network through an rpc node
 */
export declare class RpcClient {
    private url;
    private chain;
    private httpBackend;
    /**
     *
     * @param url rpc root url
     * @param chain chain (default main)
     * @param httpBackend Http backend that issue http request.
     * You can override it by providing your own if you which to hook in the request/response
     *
     * @example new RpcClient('https://api.tez.ie/rpc/mainnet', 'main') this will use https://api.tez.ie/rpc/mainnet/chains/main
     */
    constructor(url: string, chain?: string, httpBackend?: HttpBackend);
    private createURL;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description Get the block's hash, its unique identifier.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-hash
     */
    getBlockHash({ block }?: RPCOptions): Promise<string>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description List the ancestors of the given block which, if referred to as the branch in an operation header, are recent enough for that operation to be included in the current block.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-live-blocks
     */
    getLiveBlocks({ block }?: RPCOptions): Promise<string[]>;
    /**
     *
     * @param address address from which we want to retrieve the balance
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the balance of a contract.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-balance
     */
    getBalance(address: string, { block }?: RPCOptions): Promise<BalanceResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve the storage
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the data of the contract.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-storage
     */
    getStorage(address: string, { block }?: {
        block: string;
    }): Promise<StorageResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve the script
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the code and data of the contract.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-script
     */
    getScript(address: string, { block }?: {
        block: string;
    }): Promise<ScriptResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the complete status of a contract.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id
     */
    getContract(address: string, { block }?: {
        block: string;
    }): Promise<ContractResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve the manager
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the manager key of a contract.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-manager-key
     */
    getManagerKey(address: string, { block }?: {
        block: string;
    }): Promise<ManagerKeyResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve the delegate (baker)
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the delegate of a contract, if any.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-delegate
     */
    getDelegate(address: string, { block }?: {
        block: string;
    }): Promise<DelegateResponse>;
    /**
     *
     * @param address contract address from which we want to retrieve the big map key
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the value associated with a key in the big map storage of the contract.
     *
     * @deprecated Deprecated in favor of getBigMapKeyByID
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-script
     */
    getBigMapKey(address: string, key: BigMapKey, { block }?: {
        block: string;
    }): Promise<BigMapGetResponse>;
    /**
     *
     * @param id Big Map ID
     * @param expr Expression hash to query (A b58check encoded Blake2b hash of the expression (The expression can be packed using the pack_data method))
     * @param options contains generic configuration for rpc calls
     *
     * @description Access the value associated with a key in a big map.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-big-maps-big-map-id-script-expr
     */
    getBigMapExpr(id: string, expr: string, { block }?: {
        block: string;
    }): Promise<BigMapResponse>;
    /**
     *
     * @param address delegate address which we want to retrieve
     * @param options contains generic configuration for rpc calls
     *
     * @description Fetches information about a delegate from RPC.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-delegates-pkh
     */
    getDelegates(address: string, { block }?: {
        block: string;
    }): Promise<DelegatesResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description All constants
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-constants
     */
    getConstants({ block }?: RPCOptions): Promise<ConstantsResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls. See examples for various available sytaxes.
     *
     * @description All the information about a block
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id
     * @example getBlock() will default to /main/chains/block/head.
     * @example getBlock({ block: head~2 }) will return an offset of 2 blocks.
     * @example getBlock({ block: BL8fTiWcSxWCjiMVnDkbh6EuhqVPZzgWheJ2dqwrxYRm9AephXh~2 }) will return an offset of 2 blocks from given block hash..
     */
    getBlock({ block }?: RPCOptions): Promise<BlockResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description The whole block header
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-header
     */
    getBlockHeader({ block }?: RPCOptions): Promise<BlockHeaderResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description All the metadata associated to the block
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-metadata
     */
    getBlockMetadata({ block }?: RPCOptions): Promise<BlockMetadata>;
    /**
     *
     * @param args contains optional query arguments
     * @param options contains generic configuration for rpc calls
     *
     * @description Retrieves the list of delegates allowed to bake a block.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-helpers-baking-rights
     */
    getBakingRights(args?: BakingRightsQueryArguments, { block }?: RPCOptions): Promise<BakingRightsResponse>;
    /**
     *
     * @param args contains optional query arguments
     * @param options contains generic configuration for rpc calls
     *
     * @description Retrieves the list of delegates allowed to bake a block.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-helpers-endorsing-rights
     */
    getEndorsingRights(args?: EndorsingRightsQueryArguments, { block }?: RPCOptions): Promise<EndorsingRightsResponse>;
    /**
     * @param options contains generic configuration for rpc calls
     *
     * @description Ballots casted so far during a voting period
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-ballot-list
     */
    getBallotList({ block }?: RPCOptions): Promise<BallotListResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description Sum of ballots casted so far during a voting period.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-ballots
     */
    getBallots({ block }?: RPCOptions): Promise<BallotsResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description Current period kind.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-current-period-kind
     */
    getCurrentPeriodKind({ block }?: RPCOptions): Promise<PeriodKindResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description Current proposal under evaluation.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-current-proposal
     */
    getCurrentProposal({ block }?: RPCOptions): Promise<CurrentProposalResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description Current expected quorum.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-current-quorum
     */
    getCurrentQuorum({ block }?: RPCOptions): Promise<CurrentQuorumResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description List of delegates with their voting weight, in number of rolls.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-listings
     */
    getVotesListings({ block }?: RPCOptions): Promise<VotesListingsResponse>;
    /**
     *
     * @param options contains generic configuration for rpc calls
     *
     * @description List of proposals with number of supporters.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-votes-proposals
     */
    getProposals({ block }?: RPCOptions): Promise<ProposalsResponse>;
    /**
     *
     * @param data operation contents to forge
     * @param options contains generic configuration for rpc calls
     *
     * @description Forge an operation returning the unsigned bytes
     *
     * @see https://tezos.gitlab.io/api/rpc.html#post-block-id-helpers-forge-operations
     */
    forgeOperations(data: ForgeOperationsParams, { block }?: RPCOptions): Promise<string>;
    /**
     *
     * @param signedOpBytes signed bytes to inject
     *
     * @description Inject an operation in node and broadcast it. Returns the ID of the operation. The `signedOperationContents` should be constructed using a contextual RPCs from the latest block and signed by the client. By default, the RPC will wait for the operation to be (pre-)validated before answering. See RPCs under /blocks/prevalidation for more details on the prevalidation context.
     *
     * @see https://tezos.gitlab.io/api/rpc.html#post-injection-operation
     */
    injectOperation(signedOpBytes: string): Promise<OperationHash>;
    /**
     *
     * @param ops Operations to apply
     * @param options contains generic configuration for rpc calls
     *
     * @description Simulate the validation of an operation
     *
     * @see https://tezos.gitlab.io/api/rpc.html#post-block-id-helpers-preapply-operations
     */
    preapplyOperations(ops: PreapplyParams, { block }?: RPCOptions): Promise<PreapplyResponse[]>;
    /**
     *
     * @param contract address of the contract we want to get the entrypoints of
     *
     * @description Return the list of entrypoints of the contract
     *
     * @see https://tezos.gitlab.io/api/rpc.html#get-block-id-context-contracts-contract-id-entrypoints
     *
     * @version 005_PsBABY5H
     */
    getEntrypoints(contract: string, { block }?: RPCOptions): Promise<EntrypointsResponse>;
    /**
     * @param op Operation to run
     * @param options contains generic configuration for rpc calls
     *
     * @description Run an operation without signature checks
     *
     * @see https://tezos.gitlab.io/api/rpc.html#post-block-id-helpers-scripts-run-operation
     */
    runOperation(op: RPCRunOperationParam, { block }?: RPCOptions): Promise<PreapplyResponse>;
    getChainId(): Promise<string>;
    /**
     *
     * @param data Data to pack
     * @param options contains generic configuration for rpc calls
     *
     * @description Computes the serialized version of a data expression using the same algorithm as script instruction PACK
     *
     * @example packData({ data: { string: "test" }, type: { prim: "string" } })
     *
     * @see https://tezos.gitlab.io/api/rpc.html#post-block-id-helpers-scripts-pack-data
     */
    packData(data: PackDataParams, { block }?: RPCOptions): Promise<{
        packed: string;
        gas: BigNumber | "unaccounted" | undefined;
    }>;
    /**
     *
     * @description Return rpc root url
     */
    getRpcUrl(): string;
}
