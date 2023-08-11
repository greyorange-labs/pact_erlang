pact-erlang
=====

An erlang library for contract testing using pact ffi and generating consumer pacts.

This library should be considered alpha quality. It is not yet feature complete and the API is subject to change.

Docs: https://hexdocs.pm/pact_erlang/readme.html

Build
-----

    $ make


Add pact-erlang as a dependency in your application
---------------------------------------------------
```
{deps, [pact_erlang]}.
```

Usage
-----


```erlang
%% Setup
%% Define pact consumer and producer
PactRef = pact:v4(<<"consumer">>, <<"producer">>).

%% Define the interaction, returns running mock server port
{ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"/users api desc">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/users">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => jsx:encode(#{user_id => 1, user_name => <<"ranjan">>, age => 26})
        }
    }).


%% test your code which calls the api
Users = user:get_users(<<"127.0.0.1">>, Port).


%% Verify if everything matched successfully
assertEqual({ok, matched}, pact:verify(PactRef)).


%% Should write Pact File if matched
pact:write(PactRef, "/path/to/pacts")

%% Cleanup test setup
%% This won't cleanup the pact files, only the pact ref you created in the test setup
pact:cleanup(PactRef)
```
Matching request path and request/response headers, and body values
-----

For now the matchers module is not implemented, but we can very much match values.
Refer source - https://github.com/pact-foundation/pact-reference/blob/master/rust/pact_ffi/IntegrationJson.md
