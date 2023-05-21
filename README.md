pact_erlang
=====

An OTP library

Build
-----

    $ rebar3 compile


Usage
-----


```erlang
%% Setup
%% Define pact consumer and producer
PactRef = pact:create_new_pact(<<"consumer">>, <<"producer">>).

%% Define the interaction description associated with a pact ref
InteractionRef = pact:create_new_interaction(PactRef, <<"/users api desc">>).

%% Define interaction's request method, path, response content type and body as well as response code
pact:with_request(InteractionRef, <<"GET">>, <<"/users">>).
ResponseJsonString = jsx:encode(#{auth_id => 1}).
pact:with_response_body(InteractionRef, <<"application/json">>, ResponseJsonString).
pact:with_response_status(InteractionRef, 200).

%% Create the mock producer
pact:create_mock_server_for_transport(PactRef, <<"localhost">>, 1234, <<"http">>).


%% test your code which calls the api
Users = user:get_users(),


%% Verify if everything matched successfully
assertEqual({ok, matched}, pact:verify(1234)).


%% Should write Pact File if matched
pact:write_pact_file(PactRef, "/Users/priyaranjan.m/Work/grey_orange/greymatter-api-docs/pact-erlang-poc/consumer_server/pacts", 0)

%% Cleanup test setup
%% This won't cleanup the pact files, only the pact ref you created in the test setup
pact:cleanup_mock_server()
pact:cleanup_pact()
```
