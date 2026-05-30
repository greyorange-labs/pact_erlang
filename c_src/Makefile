CC := gcc
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)
ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
LIBRARY_BASE_PATH := $(BASEDIR)/priv
PACT_INCLUDE_PATH := $(CURDIR)/include

UNAME := $(shell uname -s)
ARCHITECTURE := $(shell uname -m)

PACT_FFI_VERSION  := 0.4.27
PACT_FFI_BASE_URL := https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v$(PACT_FFI_VERSION)
PACT_H_URL        := $(PACT_FFI_BASE_URL)/pact.h

# Normalise the machine arch to pact's naming (arm64/aarch64 -> aarch64).
ifeq ($(ARCHITECTURE), x86_64)
    PACT_ARCH := x86_64
else
    PACT_ARCH := aarch64
endif

ifeq ($(UNAME),Linux)
    CFLAGS := -O2 -Wmissing-prototypes -Wall -shared -o
    LIBC := $(shell $(CC) -dumpmachine 2>/dev/null | grep -q musl && echo musl || echo glibc)
    ifeq ($(LIBC), musl)
        LIBC_SUFFIX := -musl
    endif
    PACT_FILE_NAME := libpact_ffi-linux-$(PACT_ARCH)$(LIBC_SUFFIX).a
endif
ifeq ($(UNAME),Darwin)
    CFLAGS := -undefined dynamic_lookup -shared -o
    PACT_FILE_NAME := libpact_ffi-macos-$(PACT_ARCH).a
endif
PACT_FFI_URL := $(PACT_FFI_BASE_URL)/$(PACT_FILE_NAME).gz

SRC := pactffi_nif.c
TARGET := $(BASEDIR)/priv/libpact_ffi.so
LIBRARY_PATH := $(LIBRARY_BASE_PATH)/$(PACT_FILE_NAME)
PACT_VERSION_FILE := $(LIBRARY_BASE_PATH)/.pact-ffi-version-$(PACT_FFI_VERSION)

PVER_EXEC_SRC := pact_verifier_external.c
PVER_EXEC_BIN := $(LIBRARY_BASE_PATH)/pact_verifier_external

ifeq ($(UNAME),Linux)
all: $(TARGET) $(PVER_EXEC_BIN) compile_commands.json
else
all: $(TARGET) compile_commands.json
endif
	@:

# Generate compile_commands.json without using external commands
compile_commands.json: Makefile
	@echo "[{" > compile_commands.json
	@echo "  \"directory\": \"$(CURDIR)\"," >> compile_commands.json
	@echo "  \"file\": \"$(CURDIR)/$(SRC)\"," >> compile_commands.json
	@echo "  \"command\": \"$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH)\"" >> compile_commands.json
	@echo "}]" >> compile_commands.json

$(TARGET): $(SRC) $(LIBRARY_PATH)
	@$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH) -lpthread -ldl

ifeq ($(UNAME),Linux)
$(PVER_EXEC_BIN): $(PVER_EXEC_SRC) $(LIBRARY_PATH)
	@$(CC) -o $(PVER_EXEC_BIN) ../c_src/$(PVER_EXEC_SRC) -I../c_src/include $(LIBRARY_PATH) -lm -lpthread -ldl
endif

$(LIBRARY_PATH): $(PACT_VERSION_FILE)
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_PATH)
	@rm -f $(LIBRARY_PATH).gz
	@echo "Fetching libpact_ffi..."
	@curl -sS -L $(PACT_FFI_URL) -o $(LIBRARY_PATH).gz
	@gzip -d $(LIBRARY_PATH).gz
	@echo "Done"

$(PACT_VERSION_FILE):
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_BASE_PATH)/.pact-ffi-version-*
	@touch $(PACT_VERSION_FILE)

clean:
	@rm -f $(TARGET)
	@rm -f compile_commands.json
	@rm -f $(LIBRARY_PATH)
	@rm -f $(PACT_VERSION_FILE)

update-pact-header:
	@rm -f $(PACT_INCLUDE_PATH)/pact.h
	@echo "Fetching pact.h..."
	@curl -sS -L $(PACT_H_URL) -o $(PACT_INCLUDE_PATH)/pact.h
	@echo "Done"
	@echo ">> Please commit the include/pact.h file if there are any updates"

.PHONY: all clean update-pact-header
