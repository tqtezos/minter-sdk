.PHONY: swap-storage clean

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile-storage --syntax cameligo

# Folder with modules
MODULES ?= ../..

# Output folder
BIN ?= ../../../bin

all: \
	$(BIN)/fa2_swap.tz

$(BIN)/fa2_swap.tz: fa2_swap.mligo $(MODULES)/fa2/**.mligo $(MODULES)/fa2_modules/**.mligo
	$(BUILD) fa2_swap.mligo swaps_main --output-file $(BIN)/fa2_swap.tz

clean:
	rm $(BIN)/fa2_swap.tz
