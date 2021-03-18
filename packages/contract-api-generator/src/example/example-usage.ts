import { TezosToolkit } from '@taquito/taquito';
import { Contract as TestContractType } from './example-contract-type-1';
import { Contract as TestContractType2 } from './example-contract-type-2';
import { nat, tas } from '../usage/type-aliases';
import { TezosToolkitTyped } from '../usage/tezos-toolkit-typing';


const exampleContractMethods1 = async () => {

    // TODO: Support Generic Type in TezosToolkit Directly
    // const Tezos = new TezosToolkit<TestContractType>(`https://YOUR_PREFERRED_RPC_URL`)
    const Tezos = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType>;

    const contract = await Tezos.contract.at(`tz123`);
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

};

const exampleContractMethods2 = async () => {

    // TODO: Support Generic Type in TezosToolkit Directly
    // const Tezos = new TezosToolkit<TestContractType2>(`https://YOUR_PREFERRED_RPC_URL`)
    const Tezos = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType2>;

    const originationResult = await Tezos.contract.originate({
        code: ``,
        storage: {},
    });
    const contract = await originationResult.contract(5);
    contract.methods.set_admin(tas.address(`tz123`));
    contract.methods.create_token({
        token_id: tas.nat(`100000000000000`),
        token_info: tas.map([
            { key: `0`, value: tas.bytes(`abc`) },
            { key: `1`, value: tas.bytes(`def`) },
        ]),
    });
    contract.methods.create_token({
        token_id: tas.nat(`100000000000000`),
        token_info: tas.map({
            0: tas.bytes(`abc`),
            1: tas.bytes(`def`),
        }),
    });

};


const exampleContractStorage1 = async () => {

    // TODO: Support Generic Type in TezosToolkit Directly
    // const Tezos = new TezosToolkit<TestContractType>(`https://YOUR_PREFERRED_RPC_URL`)
    const Tezos = new TezosToolkit(`https://YOUR_PREFERRED_RPC_URL`) as unknown as TezosToolkitTyped<TestContractType>;

    const contract = await Tezos.contract.at(``);

    const getAuctionInfo = async (id: nat) => {
        const storage = await contract.storage();

        const auctions = storage.auctions;
        const auction = await auctions.get(id);
        if (!auction) {
            throw new Error(`Auction is missing`);
        }
        return auction;
    };

    const auctionId = tas.nat(42);

    // Get current bid
    const { current_bid } = await getAuctionInfo(auctionId);

    // Make next bid
    await (await contract.methods.bid(auctionId).send({
        mutez: true,
        amount: tas.add(current_bid, tas.mutez(1000)),
    })).confirmation(100);

    // Get current owner
    const { highest_bidder } = await getAuctionInfo(auctionId);
    const userAddress = await Tezos.wallet.pkh();

    if (highest_bidder === userAddress) {
        console.log(`You are the highest bidder!`);
    }

};