Pact Erlang
===========

![Build Status](https://github.com/greyorange-labs/pact_erlang/actions/workflows/erlang.yml/badge.svg?event=push)
[![Code Coverage](https://codecov.io/gh/greyorange-labs/pact_erlang/branch/develop/graph/badge.svg?token=9F8XCB1TBR)](https://codecov.io/gh/greyorange-labs/pact_erlang)
[![Hex Version](https://img.shields.io/hexpm/v/pact_erlang.svg)](https://hex.pm/packages/pact_erlang)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/pact_erlang/)
[![Total Download](https://img.shields.io/hexpm/dt/pact_erlang.svg)](https://hex.pm/packages/pact_erlang)
[![License](https://img.shields.io/hexpm/l/pact_erlang.svg)](https://github.com/greyorange-labs/pact_erlang/blob/develop/LICENSE)

An Erlang library for contract testing using Pact FFI, supporting both HTTP APIs and asynchronous messaging systems. This library enables Erlang applications to participate in consumer-driven contract testing by generating and verifying Pact contracts.

📚 **Documentation:** [hexdocs.pm/pact_erlang](https://hexdocs.pm/pact_erlang)  
📋 **Changelog:** [changelog.html](https://hexdocs.pm/pact_erlang/changelog.html)  
🏗️ **Architecture:** [ARCHITECTURE.md](ARCHITECTURE.md)  
🤝 **Contributing:** [CONTRIBUTING.md](CONTRIBUTING.md)

Features
--------

- **HTTP Contract Testing**: Create and verify HTTP API contracts
- **Message Contract Testing**: Test asynchronous messaging interactions
- **Consumer & Provider Testing**: Support for both sides of contract testing
- **Flexible Matching**: Type matching, regex matching, and custom matchers
- **Pact Broker Integration**: Publish and fetch contracts from Pact Broker
- **Multiple Platforms**: Support for Linux (x86_64, aarch64) and macOS (x86_64, arm64)
- **Built on Pact FFI**: Leverages the mature Rust Pact FFI library

Quick Start
-----------

### Installation

Add `pact_erlang` as a dependency in your `rebar.config`:

~~~erlang
{deps, [pact_erlang]}.
~~~

### Build

~~~bash
make
~~~

API Reference
-------------

### Core Consumer APIs

#### `pact:v4/2` - Create a Pact Contract

Creates a new Pact contract between a consumer and provider.

~~~erlang
PactRef = pact:v4(Consumer, Provider).
~~~

**Parameters:**

- `Consumer` - Binary string identifying the consumer application
- `Provider` - Binary string identifying the provider application

**Returns:** Process ID (PactRef) representing the Pact contract

**Example:**

~~~erlang
PactRef = pact:v4(<<"my_app">>, <<"user_service">>).
~~~

#### `pact:interaction/2` - Define HTTP Interaction

Creates an HTTP interaction specification and starts a mock server.

~~~erlang
{ok, Port} = pact:interaction(PactRef, InteractionSpec).
~~~

**Parameters:**

- `PactRef` - Process ID returned by `pact:v4/2`
- `InteractionSpec` - Map containing interaction details

**Returns:** `{ok, Port}` where Port is the mock server port

**InteractionSpec Structure:**

~~~erlang
#{
    given => ProviderState,              % Optional: Provider state
    upon_receiving => Description,       % Required: Interaction description  
    with_request => RequestSpec,         % Required: Request specification
    will_respond_with => ResponseSpec    % Required: Response specification
}
~~~

**Provider State Options:**

~~~erlang
% Simple state
given => <<"user exists">>

% State with parameters  
given => #{
    state => <<"user exists">>,
    params => #{<<"userId">> => <<"123">>}
}
~~~

**Request Specification:**

~~~erlang
with_request => #{
    method => <<"GET">>,                % HTTP method
    path => <<"/users/123">>,           % Request path
    headers => #{                       % Optional: Request headers
        <<"Authorization">> => <<"Bearer token">>,
        <<"Content-Type">> => <<"application/json">>
    },
    query_params => #{                  % Optional: Query parameters
        <<"active">> => <<"true">>
    },
    body => RequestBody                 % Optional: Request body
}
~~~

**Response Specification:**

~~~erlang
will_respond_with => #{
    status => 200,                      % HTTP status code
    headers => #{                       % Optional: Response headers
        <<"Content-Type">> => <<"application/json">>
    },
    body => ResponseBody                % Optional: Response body
}
~~~

**Complete Example:**

~~~erlang
PactRef = pact:v4(<<"my_app">>, <<"user_service">>),

{ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            state => <<"user exists">>,
            params => #{<<"userId">> => <<"123">>}
        },
        upon_receiving => <<"get user by ID">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/users/123">>,
            headers => #{
                <<"Authorization">> => <<"Bearer token">>
            }
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{
                <<"id">> => <<"123">>,
                <<"name">> => <<"John Doe">>,
                <<"email">> => <<"john@example.com">>
            }
        }
    }),

% Make request to mock server
Response = http_client:get("http://127.0.0.1:" ++ integer_to_list(Port) ++ "/users/123").
~~~

#### `pact:msg_interaction/2` - Define Message Interaction

Creates a message interaction for asynchronous messaging contracts.

~~~erlang
TestMessage = pact:msg_interaction(PactRef, MessageSpec).
~~~

**Parameters:**

- `PactRef` - Process ID returned by `pact:v4/2`
- `MessageSpec` - Map containing message interaction details

**Returns:** Map containing the reified message contents

**MessageSpec Structure:**

~~~erlang
#{
    given => ProviderState,              % Optional: Provider state
    upon_receiving => Description,       % Required: Message description
    with_contents => MessageContents     % Required: Message contents
}
~~~

**Example:**

~~~erlang
PactRef = pact:v4(<<"weather_consumer">>, <<"weather_service">>),

Message = pact:like(#{
    weather => #{
        temperature => 23.0,
        humidity => 75.5,
        wind_speed_kmh => 29
    },
    timestamp => <<"2024-03-14T10:22:13+05:30">>
}),

TestMessage = pact:msg_interaction(PactRef, #{
    given => <<"weather data available">>,
    upon_receiving => <<"weather update message">>,
    with_contents => Message
}),

#{<<"contents">> := MessageContents} = TestMessage,
% Process the test message
ok = my_message_handler:process(MessageContents).
~~~

#### `pact:verify/1` - Verify Interactions

Verifies that all defined interactions were successfully matched during testing.

~~~erlang
Result = pact:verify(PactRef).
~~~

**Parameters:**

- `PactRef` - Process ID returned by `pact:v4/2`

**Returns:**

- `{ok, matched}` - All interactions matched successfully
- `{error, not_matched}` - One or more interactions failed to match

**Example:**

~~~erlang
case pact:verify(PactRef) of
    {ok, matched} ->
        io:format("All interactions verified successfully~n");
    {error, not_matched} ->
        io:format("Some interactions failed verification~n")
end.
~~~

#### `pact:write/1` and `pact:write/2` - Write Pact Files

Writes the Pact contract to a file after successful verification.

~~~erlang
pact:write(PactRef).
pact:write(PactRef, Directory).
~~~

**Parameters:**

- `PactRef` - Process ID returned by `pact:v4/2`
- `Directory` - Optional: Custom directory path (defaults to `"./pacts"`)

**Returns:** `ok`

**Example:**

~~~erlang
% Write to default directory (./pacts)
pact:write(PactRef).

% Write to custom directory
pact:write(PactRef, <<"/tmp/my_pacts">>).
~~~

#### `pact:cleanup/1` - Cleanup Resources

Cleans up resources used by the Pact contract (does not remove Pact files).

~~~erlang
pact:cleanup(PactRef).
~~~

**Parameters:**

- `PactRef` - Process ID returned by `pact:v4/2`

**Returns:** `ok`

### Matching Functions

These functions create matching rules for flexible contract testing.

#### `pact:like/1` - Type Matching

Matches values based on their type rather than exact value.

~~~erlang
Matcher = pact:like(Value).
~~~

**Example:**

~~~erlang
Body = #{
    <<"user_id">> => pact:like(123),           % Matches any integer
    <<"name">> => pact:like(<<"John">>),       % Matches any string  
    <<"active">> => pact:like(true),           % Matches any boolean
    <<"metadata">> => pact:like(#{             % Matches map with same structure
        <<"created">> => <<"2023-01-01">>
    })
}.
~~~

#### `pact:each_like/1` - Array Type Matching

Matches arrays where each element matches the provided template.

~~~erlang
Matcher = pact:each_like(Template).
~~~

**Example:**

~~~erlang
UsersArray = pact:each_like(#{
    <<"id">> => 1,
    <<"name">> => <<"User Name">>,
    <<"email">> => <<"user@example.com">>
}),
% Matches arrays of user objects with the same structure
~~~

#### `pact:regex_match/2` - Regular Expression Matching

Matches values against a regular expression pattern.

~~~erlang
Matcher = pact:regex_match(ExampleValue, RegexPattern).
~~~

**Example:**

~~~erlang
EmailMatcher = pact:regex_match(
    <<"test@example.com">>, 
    <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>
),

PhoneMatcher = pact:regex_match(
    <<"555-1234">>,
    <<"^\\d{3}-\\d{4}$">>
).
~~~

#### `pact:each_key/2` - Key Pattern Matching

Matches map keys against a regular expression pattern.

~~~erlang
Matcher = pact:each_key(ExampleValue, KeyPattern).
~~~

**Example:**

~~~erlang
% Match dynamic keys that follow a pattern
DynamicData = pact:each_key(#{
    <<"user-123">> => <<"John Doe">>,
    <<"user-456">> => <<"Jane Doe">>
}, <<"^user-\\d+$">>).
~~~

### Provider/Verifier APIs

#### `pact_verifier:start_verifier/2` - Start Provider Verifier

Starts a verifier instance for provider-side contract testing.

~~~erlang
{ok, VerifierRef} = pact_verifier:start_verifier(ProviderName, ProviderOpts).
~~~

**Parameters:**

- `ProviderName` - Binary string identifying the provider
- `ProviderOpts` - Map containing provider configuration

**ProviderOpts Structure:**

~~~erlang
#{
    name => ProviderName,                % Provider name
    version => ProviderVersion,          % Provider version
    scheme => <<"http">>,                % URL scheme (http/https)
    host => <<"localhost">>,             % Provider host
    port => 8080,                        % Provider port (optional)
    base_url => <<"/">>,                 % Base URL path
    branch => <<"main">>,                % Git branch name
    protocol => <<"http">>,              % Protocol type (http/message)
    pact_source_opts => SourceOpts,      % Pact source configuration
    message_providers => MessageProviders, % Message provider mappings
    state_change_url => StateChangeUrl   % Provider state change URL
}
~~~

**Pact Source Options:**

~~~erlang
% File-based verification
pact_source_opts => #{
    file_path => <<"./pacts">>
}

% Broker-based verification
pact_source_opts => #{
    broker_url => <<"http://pact-broker.example.com"\>\>,
    broker_username => <<"username">>,
    broker_password => <<"password">>,
    enable_pending => 1,
    consumer_version_selectors => <<"{}">>  % JSON string
}
~~~

**Message Providers:**

~~~erlang
message_providers => #{
    <<"weather update message">> => {weather_service, generate_weather, []},
    <<"user notification">> => {notification_service, create_notification, [user_id]}
},
fallback_message_provider => {default_service, generate_default, []}
~~~

**Example:**

~~~erlang
ProviderOpts = #{
    name => <<"user_service">>,
    version => <<"1.0.0">>,
    scheme => <<"http">>,
    host => <<"localhost">>,
    port => 8080,
    base_url => <<"/">>,
    branch => <<"main">>,
    protocol => <<"http">>,
    pact_source_opts => #{
        broker_url => <<"http://localhost:9292"\>\>,
        broker_username => <<"pact_user">>,
        broker_password => <<"pact_pass">>,
        enable_pending => 1,
        consumer_version_selectors => <<"{}">>
    }
},

{ok, VerifierRef} = pact_verifier:start_verifier(<<"user_service">>, ProviderOpts).
~~~

#### `pact_verifier:verify/1` - Run Verification

Executes the verification process against the provider.

~~~erlang
Result = pact_verifier:verify(VerifierRef).
~~~

**Returns:** Integer result code (0 = success, non-zero = failure)

#### `pact_verifier:verify_v2/1` - Extended Verification

Runs verification with detailed logging information.

~~~erlang
{Result, FileLog, BrokerLog} = pact_verifier:verify_v2(VerifierRef).
~~~

**Returns:** Tuple containing result code and log information

#### `pact_verifier:stop_verifier/1` - Stop Verifier

Stops the verifier and releases resources.

~~~erlang
pact_verifier:stop_verifier(VerifierRef).
~~~

### Utility APIs

#### `pact:enable_logging/1` and `pact:enable_logging/2` - Enable Logging

Enables Pact FFI logging for debugging purposes.

~~~erlang
pact:enable_logging(LogLevel).
pact:enable_logging(FilePath, LogLevel).
~~~

**Parameters:**

- `LogLevel` - Atom: `off`, `error`, `warn`, `info`, `debug`, `trace`
- `FilePath` - Binary: Custom log file path (defaults to `"./pact_erlang.log"`)

**Example:**

~~~erlang
% Enable debug logging to default file
pact:enable_logging(debug).

% Enable trace logging to custom file
pact:enable_logging(<<"/tmp/pact_debug.log">>, trace).
~~~

Complete Workflow Examples
--------------------------

### Consumer Test Example

~~~erlang
-module(user_service_consumer_test).
-include_lib("eunit/include/eunit.hrl").

user_service_test() ->
    % Setup Pact contract
    PactRef = pact:v4(<<"my_app">>, <<"user_service">>),
    
    % Define interaction
    {ok, Port} = pact:interaction(PactRef, #{
        given => <<"user 123 exists">>,
        upon_receiving => <<"get user request">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/users/123">>,
            headers => #{
                <<"Authorization">> => <<"Bearer token123">>
            }
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{
                <<"id">> => pact:like(123),
                <<"name">> => pact:like(<<"John Doe">>),
                <<"email">> => pact:regex_match(
                    <<"john@example.com">>,
                    <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>
                )
            }
        }
    }),
    
    % Test your code against the mock server
    BaseUrl = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, User} = my_user_client:get_user(BaseUrl, "123", "token123"),
    
    % Verify interaction was matched
    ?assertEqual({ok, matched}, pact:verify(PactRef)),
    
    % Write Pact file
    pact:write(PactRef),
    
    % Cleanup
    pact:cleanup(PactRef).
~~~

### Message Consumer Test Example

~~~erlang
-module(weather_consumer_test).
-include_lib("eunit/include/eunit.hrl").

weather_message_test() ->
    % Setup Pact contract
    PactRef = pact:v4(<<"weather_app">>, <<"weather_service">>),
    
    % Define expected message structure
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            conditions => <<"sunny">>
        },
        location => #{
            city => <<"San Francisco">>,
            country => <<"US">>
        },
        timestamp => pact:regex_match(
            <<"2024-03-14T10:22:13Z">>,
            <<"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$">>
        )
    }),
    
    % Create message interaction
    TestMessage = pact:msg_interaction(PactRef, #{
        given => <<"weather data is available">>,
        upon_receiving => <<"weather update message">>,
        with_contents => Message
    }),
    
    % Extract and test message contents
    #{<<"contents">> := MessageContents} = TestMessage,
    ?assertMatch(ok, weather_handler:process_weather_update(MessageContents)),
    
    % Write Pact file
    pact:write(PactRef),
    
    % Cleanup
    pact:cleanup(PactRef).
~~~

### Provider Verification Example

~~~erlang
-module(user_service_provider_test).
-include_lib("eunit/include/eunit.hrl").

verify_contracts_test() ->
    % Start your provider service
    {ok, _} = application:start(user_service),
    
    % Configure verifier
    ProviderOpts = #{
        name => <<"user_service">>,
        version => <<"1.0.0">>,
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        base_url => <<"/">>,
        branch => <<"main">>,
        protocol => <<"http">>,
        pact_source_opts => #{
            file_path => <<"./pacts">>
        },
        state_change_url => <<"http://localhost:8080/provider-states"\>\>
    },
    
    % Start verifier
    {ok, VerifierRef} = pact_verifier:start_verifier(<<"user_service">>, ProviderOpts),
    
    % Run verification
    {Result, FileLog, _BrokerLog} = pact_verifier:verify_v2(VerifierRef),
    
    % Check results
    ?assertEqual(0, Result),  % 0 means success
    
    % Stop verifier
    pact_verifier:stop_verifier(VerifierRef),
    
    % Stop provider service
    application:stop(user_service).
~~~

Advanced Usage
--------------

### Complex Matching Example

~~~erlang
% Define pact consumer and producer
PactRef = pact:v4(<<"consumer">>, <<"producer">>).

% Define the interaction, returns running mock server port
{ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            state => <<"a user ranjan exists">>
        },
        upon_receiving => <<"get all users">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/users">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{users => [#{user_id => 1, user_name => <<"ranjan">>, age => 26}]}
        }
    }).

% test your code which calls the api
Users = user:get_users(<<"127.0.0.1">>, Port).

% Verify if everything matched successfully
assertEqual({ok, matched}, pact:verify(PactRef)).

% Should write pact file if matched, creates a new folder `pacts'
% and writes the pact file inside it.
pact:write(PactRef).

% Alternatively, one can override the default pacts directory path
pact:write(PactRef, "/path/to/pacts").

% Cleanup test setup
% This won't cleanup the pact files, only the pact ref you created in the test setup
pact:cleanup(PactRef).
~~~

### Message Pacts Usage

~~~erlang
PactRef = pact:v4(<<"animal_service">>, <<"weather_service">>),

Message = pact:like(#{
    weather => #{
        temperature => 23.0,
        humidity => 75.5,
        wind_speed_kmh => 29
    },
    timestamp => <<"2024-03-14T10:22:13+05:30">>
}),
TestMessage = pact:msg_interaction(PactRef,
#{
    given => <<"weather data for animals">>,
    upon_receiving => <<"a weather data message">>,
    with_contents => Message
}),
#{<<"contents">> := TestMessageContents} = TestMessage,
?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
pact:write(PactRef).
~~~

### Pact Verification

~~~erlang
Name = <<"weather_service">>,
Version =  <<"default">>,
Scheme = <<"http">>,
Host = <<"localhost">>,
Path = <<"/message_pact/verify">>,
Branch = <<"develop">>,
FilePath = <<"./pacts">>,
WrongFilePath = <<"./pactss">>,
BrokerUrl = <<"http://localhost:9292/"\>\>,
WrongBrokerUrl = <<"http://localhost:8282/"\>\>,
Protocol = <<"message">>,
BrokerConfigs = #{
    broker_url => BrokerUrl,
    broker_username => <<"pact_workshop">>,
    broker_password => <<"pact_workshop">>,
    enable_pending => 1,
    consumer_version_selectors => thoas:encode(#{})
},
ProviderOpts = #{
    name => Name,
    version => Version,
    scheme => Scheme,
    host => Host,
    base_url => Path,
    branch => Branch,
    pact_source_opts => BrokerConfigs,
    message_providers => #{
        <<"a weather data message">> => {weather_service, generate_message, [23.5, 20, 75.0]}
    },
    fallback_message_provider => {weather_service, generate_message, [24.5, 20, 93.0]},
    protocol => Protocol
},
{ok, VerfierRef} = pact_verifier:start_verifier(Name, ProviderOpts),
Output = pact_verifier:verify(VerfierRef).
% or you can use the verify_v2/1 which returns logs of file and url based pacts
{Output, OutputLog1, OutputLog2} = pact_verifier:verify_v2(VerfierRef)
~~~

### Matching Request Path and Request/Response Headers, and Body Values

~~~erlang
% Alternatively, you can also match things inside each request/response
pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to create an animal: Lazgo">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/animals">>,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => #{
                <<"name">> => pact:like(<<"Lazgo">>),
                <<"type">> => pact:like(<<"dog">>)
            }
        },
        will_respond_with => #{
            status => 201
        }
    })
~~~

Release Checklist
-----------------

- Update version in `src/pact_erlang.app.src`
- Update CHANGELOG.md
- Run `rebar3 hex publish --dry-run` and make sure there are no un-intended files in included files
- Commit files, add a git tag matching the new version, and push to remote
- Run `rebar3 hex publish` to publish
