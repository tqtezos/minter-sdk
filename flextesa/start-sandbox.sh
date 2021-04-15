#!/bin/bash

IMAGE="tqtezos/flextesa:20210216"
ENTRY="edobox"

eve="$(docker run tqtezos/flextesa:20210216 edobox flextesa key eve)"

docker run --rm --name flextesa-sandbox \
    -e block_time=5 --detach -p 20000:20000 \
    "${IMAGE}" "${ENTRY}" start --add-bootstrap-account="$eve@2_000_000_000_000" \
    --no-daemons-for=eve 