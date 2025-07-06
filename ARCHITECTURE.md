# Pact Erlang Architecture

This document explains the architecture and code flow of the Pact Erlang library, which provides contract testing capabilities for Erlang applications using the Pact specification.

## Overview

Pact Erlang is a wrapper around the Pact FFI (Foreign Function Interface) library written in Rust. The library enables Erlang applications to participate in contract testing by generating and verifying Pact contracts for both HTTP APIs and asynchronous messaging systems.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Erlang Application                       │
└─────────────────────┬───────────────────────────────────────┘
                      │ Calls pact:* APIs
┌─────────────────────▼───────────────────────────────────────┐
│                Erlang Pact Library                         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │  pact.erl   │  │pact_consumer│  │  pact_verifier.erl  │ │
│  │ (Public API)│  │   .erl      │  │  (Provider side)    │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │pact_consumer│  │pact_consumer│  │ pact_ref_server.erl │ │
│  │_http.erl    │  │_msg.erl     │  │  (State manager)    │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
└─────────────────────┬───────────────────────────────────────┘
                      │ NIF calls
┌─────────────────────▼───────────────────────────────────────┐
│                 C NIF Layer                                 │
│             (pactffi_nif.c)                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  - Type conversion (Erlang ↔ C)                        │ │
│  │  - Error handling                                      │ │
│  │  - Memory management                                   │ │
│  │  - Function mapping                                    │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────┬───────────────────────────────────────┘
                      │ FFI calls
┌─────────────────────▼───────────────────────────────────────┐
│                 Pact FFI Library                           │
│                   (Rust/C)                                 │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  - Contract creation & management                       │ │
│  │  - Mock server implementation                          │ │
│  │  - Contract verification                               │ │
│  │  - Pact file generation                                │ │
│  │  - Matching algorithms                                 │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Public API Layer (`pact.erl`)

The main entry point for users, providing a simplified interface for:
- Creating Pact contracts (`v4/2`)
- Defining HTTP interactions (`interaction/2`)
- Defining message interactions (`msg_interaction/2`)
- Verifying contracts (`verify/1`)
- Writing Pact files (`write/1`, `write/2`)
- Cleanup (`cleanup/1`)
- Matching rules (`like/1`, `each_like/1`, `regex_match/2`)

### 2. Consumer Layer

#### HTTP Consumer (`pact_consumer_http.erl`)
Handles HTTP-based contract testing:
- **Interaction Creation**: Creates new HTTP interactions with request/response specifications
- **Mock Server Management**: Starts and manages mock servers for testing
- **Request/Response Processing**: Handles HTTP methods, headers, bodies, query parameters
- **Provider State Management**: Manages "given" states for interactions

#### Message Consumer (`pact_consumer_msg.erl`)
Handles asynchronous message contract testing:
- **Message Interaction Creation**: Creates message-based interactions
- **Content Processing**: Handles message contents and metadata
- **Message Reification**: Generates test messages from specifications

#### Consumer Coordinator (`pact_consumer.erl`)
Acts as a coordinator between HTTP and message consumers:
- Routes requests to appropriate consumer modules
- Provides unified interface for verification and writing
- Handles encoding of complex data structures

### 3. State Management (`pact_ref_server.erl`)

A GenServer that maintains the state of Pact contracts:
- **Pact Reference Tracking**: Maintains references to Pact FFI handles
- **Consumer/Producer Information**: Stores contract participant details
- **Mock Server Port Management**: Tracks running mock servers
- **Interaction State**: Maintains current interaction details
- **Lifecycle Management**: Handles creation and cleanup

### 4. Provider/Verifier Layer (`pact_verifier.erl`)

Handles the provider side of contract testing:
- **HTTP Server**: Embedded HTTP server for handling verification requests
- **Verification Orchestration**: Coordinates file-based and broker-based verification
- **Message Provider Support**: Handles message contract verification
- **Configuration Management**: Manages provider settings and options
- **Result Processing**: Handles verification results and reporting

### 5. NIF Interface Layer (`pactffi_nif.erl` & `pactffi_nif.c`)

#### Erlang NIF Module (`pactffi_nif.erl`)
- **Function Declarations**: Declares all NIF functions with fallback errors
- **Platform Abstraction**: Handles OS-specific differences (Linux vs. macOS)
- **Wrapper Functions**: Provides higher-level functions that process NIF results

#### C NIF Implementation (`pactffi_nif.c`)
- **Type Conversion**: Converts between Erlang terms and C types
- **Memory Management**: Handles allocation and deallocation of C structures
- **Error Handling**: Converts C errors to Erlang error tuples
- **Function Mapping**: Maps Erlang function calls to Pact FFI functions
- **Platform-Specific Code**: Handles different execution models for verification

### 6. Pact FFI Library (External Dependency)

The underlying Rust library that provides:
- **Contract Models**: Core Pact specification implementation
- **Mock Servers**: HTTP mock server implementation
- **Verification Engine**: Contract verification algorithms
- **File I/O**: Pact file reading/writing
- **Broker Communication**: Pact Broker integration
- **Matching Rules**: Content matching and validation

## Data Flow

### Consumer Workflow (Creating Contracts)

1. **Initialization**
   ```erlang
   PactRef = pact:v4(<<"consumer">>, <<"provider">>)
   ```
   - Creates a new `pact_ref_server` GenServer
   - Initializes consumer/producer information

2. **Interaction Definition**
   ```erlang
   {ok, Port} = pact:interaction(PactRef, InteractionSpec)
   ```
   - Routes to `pact_consumer_http:interaction/2` or `pact_consumer_msg:interaction/2`
   - Creates Pact and Interaction handles via NIF calls
   - For HTTP: Starts mock server and returns port
   - For messages: Returns reified message contents

3. **NIF Processing**
   - `pactffi_nif:new_pact/2` → creates Pact handle
   - `pactffi_nif:new_interaction/2` → creates Interaction handle
   - `pactffi_nif:with_request/3`, `pactffi_nif:with_body/4`, etc. → configure interaction
   - `pactffi_nif:create_mock_server_for_transport/4` → starts mock server

4. **Testing**
   - Application makes HTTP calls to mock server
   - Mock server validates requests against interaction specifications

5. **Verification & Writing**
   ```erlang
   {ok, matched} = pact:verify(PactRef),
   pact:write(PactRef)
   ```
   - Verifies all interactions matched
   - Writes Pact file to disk

### Provider Workflow (Verifying Contracts)

1. **Verifier Setup**
   ```erlang
   {ok, VerifierRef} = pact_verifier:start_verifier(Name, Opts)
   ```
   - Creates GenServer with provider configuration
   - For message protocols: Starts embedded HTTP server

2. **Verification Execution**
   ```erlang
   Result = pact_verifier:verify(VerifierRef)
   ```
   - Routes through platform-specific verification paths
   - Linux: Uses external executable via `pact_verifier_external.c`
   - macOS: Uses direct NIF calls
   - Handles both file-based and broker-based verification

3. **Message Provider Handling**
   - HTTP server receives verification requests
   - Routes to configured message providers
   - Returns generated messages for verification

## Build System

### Native Dependencies
- **Pact FFI Library**: Downloaded during build (platform-specific)
- **Header Files**: `pact.h` downloaded from Pact FFI releases
- **Static Libraries**: Architecture-specific (x86_64, aarch64, arm64)

### Compilation Process
1. **Download Dependencies**: Makefile downloads Pact FFI library and headers
2. **NIF Compilation**: Compiles C NIF against Erlang runtime and Pact FFI
3. **Executable Creation**: On Linux, creates standalone verifier executable
4. **Library Packaging**: Bundles shared library and assets in `priv/`

### Platform Differences
- **Linux**: Uses external executable for verification to avoid linking issues
- **macOS**: Uses direct NIF calls for all operations
- **Architecture Support**: x86_64 and ARM64/aarch64 on both platforms

## Error Handling

### Multi-Level Error Propagation
1. **Pact FFI Level**: Returns status codes and error messages
2. **C NIF Level**: Converts to Erlang error tuples `{error, Reason}`
3. **Erlang Level**: Propagates errors with context
4. **Application Level**: Receives meaningful error information

### Resource Management
- **Automatic Cleanup**: GenServers handle resource cleanup on termination
- **Explicit Cleanup**: `pact:cleanup/1` for manual resource management
- **Mock Server Cleanup**: Automatic cleanup of mock servers
- **Memory Management**: C NIF handles Pact FFI resource lifecycle

## Configuration

### Consumer Configuration
- Consumer/Provider names
- Interaction specifications (HTTP/Message)
- Matching rules and provider states
- Mock server settings

### Provider Configuration
- Provider name and version
- HTTP server settings (host, port, paths)
- Pact source configuration (files, broker)
- Verification options and timeouts
- Message provider mappings

## Integration Points

### Pact Broker
- **Publishing**: Automated publishing of generated Pact files
- **Verification**: Fetching contracts from broker for verification
- **Authentication**: Username/password and token-based auth
- **Version Selection**: Consumer version selectors and tagging

### CI/CD Integration
- **File-based Testing**: Local Pact file verification
- **Broker Integration**: Remote contract verification
- **Result Reporting**: Machine-readable verification results

This architecture provides a robust foundation for contract testing in Erlang applications while leveraging the mature Pact FFI implementation for core functionality.
