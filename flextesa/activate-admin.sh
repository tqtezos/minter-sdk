#!/usr/bin/env bash

# THIS FILE SHOULD NOT BE USED IN CI

TEZOS_CLIENT_LINK="https://github.com/serokell/tezos-packaging/releases/download/v8.2-2/tezos-client"

if ! command -v wget &> /dev/null
then
    echo "(wget) could not be found"
    exit
fi

if ! [ -f ../packages/minter-contracts/bin/tezos-client ]
then
    wget -O ../packages/minter-contracts/bin/tezos-client "$TEZOS_CLIENT_LINK"
    chmod +x ../packages/minter-contracts/bin/tezos-client
fi

# activate account
../packages/minter-contracts/bin/tezos-client --endpoint http://localhost:20000 \
    activate account marketplace_admin with \
    ../packages/minter-contracts/config/marketplace_admin.json &> /dev/null

balance=$(../packages/minter-contracts/bin/tezos-client --endpoint http://localhost:20000 get balance for marketplace_admin 2> /dev/null)

balanceNoSymbol=${balance::-1}

echo "marketplace_admin has $balanceNoSymbol tezzies!"

if [ $balanceNoSymbol -lt 1 ]
then
# transfer some tez from bob and alice to marketplace_admin
    ../packages/minter-contracts/bin/tezos-client --endpoint http://localhost:20000  \
        transfer 200 from alice to marketplace_admin --burn-cap 0.06425 &> /dev/null

    ../packages/minter-contracts/bin/tezos-client --endpoint http://localhost:20000  \
        transfer 200 from bob to marketplace_admin --burn-cap 0.06425 &> /dev/null
fi
