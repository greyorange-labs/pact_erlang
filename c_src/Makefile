CC := gcc
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)
ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
LIBRARY_PATH := $(BASEDIR)/priv
PACT_INCLUDE_PATH := $(CURDIR)/include

UNAME := $(shell uname -s)
ARCHITECTURE := $(shell uname -m)


ifeq ($(UNAME),Linux)
    CFLAGS := -shared -o
	ifeq ($(ARCHITECTURE), arm64)
		URL := "https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v0.4.4/libpact_ffi-linux-aarch64.a.gz"
		PACT_FILE_NAME := libpact_ffi-linux-aarch64.a.gz
		LIBRARY_PATH := $(LIBRARY_PATH)/libpact_ffi-linux-aarch64.a
	endif
	ifeq ($(ARCHITECTURE), x86_64)
		URL := "https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v0.4.4/libpact_ffi-linux-x86_64.a.gz"
		PACT_FILE_NAME := libpact_ffi-linux-x86_64.a.gz
		LIBRARY_PATH := $(LIBRARY_PATH)/libpact_ffi-linux-x86_64.a
	endif
endif
ifeq ($(UNAME),Darwin)
	CFLAGS := -undefined dynamic_lookup -shared -o
	ifeq ($(ARCHITECTURE), arm64)
		URL := "https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v0.4.4/libpact_ffi-osx-aarch64-apple-darwin.a.gz"
		PACT_FILE_NAME := libpact_ffi-osx-aarch64-apple-darwin.a.gz
    	LIBRARY_PATH := $(LIBRARY_PATH)/libpact_ffi-osx-aarch64-apple-darwin.a
	endif 
	ifeq ($(ARCHITECTURE), x86_64)
		URL := "https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v0.4.4/libpact_ffi-osx-x86_64.a.gz"
		PACT_FILE_NAME := libpact_ffi-osx-x86_64.a.gz
    	LIBRARY_PATH := $(LIBRARY_PATH)/libpact_ffi-osx-x86_64.a
	endif
endif

SRC := pact_ffi_nif.c
TARGET := $(BASEDIR)/priv/libpact_ffi.so

all: fetch_pact_lib $(TARGET)

fetch_pact_lib:
	@if [ ! -e "$(LIBRARY_PATH)" ]; then \
		mkdir -p $(BASEDIR)/priv/; \
		curl -L $(URL) > $(BASEDIR)/priv/$(PACT_FILE_NAME); \
		gzip -d $(BASEDIR)/priv/$(PACT_FILE_NAME); \
    else \
        echo "$(LIBRARY_PATH) exists. Skipping"; \
    fi
	# @mkdir -p $(BASEDIR)/priv/
	# @curl -L $(URL) > $(BASEDIR)/priv/$(PACT_FILE_NAME)
	# @gzip -d $(BASEDIR)/priv/$(PACT_FILE_NAME)

$(TARGET): $(SRC)
	@mkdir -p $(BASEDIR)/priv/
	@$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH)

clean:
	@rm -f $(TARGET)
	@rm -f $(LIBRARY_PATH)
