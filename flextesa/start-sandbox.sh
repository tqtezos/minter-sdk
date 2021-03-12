#!/bin/bash

IMAGE="tqtezos/flextesa:20210216"
ENTRY="edobox"


docker run --rm --name flextesa-sandbox \
    -e block_time=5 --detach -p 20000:20000 \
    "${IMAGE}" "${ENTRY}" start
