.PHONY: build test test-network test-network-verbose test-help clean

TEST_ARGUMENTS ?= ""

all: test

# Compile contracts necessary for tests.
compile-contracts:
	$(MAKE) -C ../ligo/src/swaps

# Build and run tests (only in emulation).
test: compile-contracts
	stack test minter-sdk --fast \
		--test-arguments "--color always $(TEST_ARGUMENTS)"

# Run tests against a real node.
# By default, the config file (e.g. in "~/.tezos-client/config") is used
# to locate the node, but those options can be overriden.
test-network: TEST_ARGUMENTS += --nettest-run-network
test-network: test

# It is possible to run tests against real network with various verbosity levels.
test-network-verbose: TEST_ARGUMENTS += -V
test-network-verbose: test-network

# See more options that can be provided via TEST_ARGUMENTS variable:
test-help:
	stack test minter-sdk --fast --test-arguments='--help'

clean:
	stack clean
