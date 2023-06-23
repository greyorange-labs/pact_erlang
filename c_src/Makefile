CC := gcc
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)
ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
LIBRARY_BASE_PATH := $(BASEDIR)/priv
PACT_INCLUDE_PATH := $(CURDIR)/include

UNAME := $(shell uname -s)
ARCHITECTURE := $(shell uname -m)

PACT_FFI_VERSION := 0.4.5
PACT_FFI_BASE_URL := https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v$(PACT_FFI_VERSION)

ifeq ($(UNAME),Linux)
    CFLAGS := -shared -o
	ifeq ($(ARCHITECTURE), arm64)
		PACT_FILE_NAME := libpact_ffi-linux-aarch64.a
	endif
	ifeq ($(ARCHITECTURE), x86_64)
		PACT_FILE_NAME := libpact_ffi-linux-x86_64.a
	endif
endif
ifeq ($(UNAME),Darwin)
	CFLAGS := -undefined dynamic_lookup -shared -o
	ifeq ($(ARCHITECTURE), arm64)
		PACT_FILE_NAME := libpact_ffi-osx-aarch64-apple-darwin.a
	endif 
	ifeq ($(ARCHITECTURE), x86_64)
		PACT_FILE_NAME := libpact_ffi-osx-x86_64.a
	endif
endif
DOWNLOAD_URL := $(PACT_FFI_BASE_URL)/$(PACT_FILE_NAME).gz

SRC := pact_ffi_nif.c
TARGET := $(BASEDIR)/priv/libpact_ffi.so
LIBRARY_PATH := $(LIBRARY_BASE_PATH)/$(PACT_FILE_NAME)
PACT_VERSION_FILE := $(LIBRARY_BASE_PATH)/.pact-ffi-version-$(PACT_FFI_VERSION)

all: $(TARGET)
	@:

$(TARGET): $(SRC) $(LIBRARY_PATH)
	@mkdir -p $(BASEDIR)/priv/
	@$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH)

$(LIBRARY_PATH): $(PACT_VERSION_FILE)
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_PATH)
	@rm -f $(LIBRARY_PATH).gz
	@echo "Fetching libpact_ffi..."
	@curl -sS -L $(DOWNLOAD_URL) -o $(LIBRARY_PATH).gz
	@gzip -d $(LIBRARY_PATH).gz
	@echo "Done"

$(PACT_VERSION_FILE):
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_BASE_PATH)/.pact-ffi-version-*
	@touch $(PACT_VERSION_FILE)

clean:
	@rm -f $(TARGET)
	@rm -f $(LIBRARY_PATH)
	@rm -f $(PACT_VERSION_FILE)

.PHONY: all clean
