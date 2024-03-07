-module(pact).

-export([
    v4/2,
    interaction/2,
    verify/1,
    write/1,
    write/2,
    cleanup/1,
    like/1,
    each_like/1,
    regex_match/2,
    each_key/2
]).

-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type pact_mock_server_port() :: integer().

%% @doc Starts a new pact server and returns its pid
%% Returns old instance's pid in case cleanup was not done correctly
-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    pact_consumer:v4(Consumer, Provider).

%% @doc Creates a mock server with the given interaction details
%% Returns its port for running pact consumer tests
-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
interaction(PactPid, Interaction) ->
    pact_consumer:interaction(PactPid, Interaction).

%% @doc Verifies Writes pact file and also finally cleanups
-spec verify(pact_pid()) -> {ok, matched} | {error, not_matched}.
verify(PactPid) ->
    pact_consumer:verify_interaction(PactPid).

%% @doc writes pact to the default directory path `./pacts'
-spec write(pact_pid()) -> ok.
write(PactPid) ->
    pact:write(PactPid, <<"./pacts">>).

%% @doc writes pact to the given directory path
-spec write(pact_pid(), binary()) -> ok.
write(PactPid, Path) ->
    pact_consumer:write_interaction(PactPid, Path).

%% @doc Stops pact server
-spec cleanup(pact_pid()) -> ok.
cleanup(PactPid) ->
    pact_consumer:cleanup(PactPid).

%% @doc Matches all the child objects (and their child objects etc.)
%% Matched according to their types
-spec like(any()) -> map().
like(Term) ->
    pact_matchers:like(Term).

%% @doc Asserts the Term is an array type that consists of elements
%% Like the one passed in
-spec like(any()) -> map().
each_like(Term) ->
    pact_matchers:each_like(Term).

%% @doc Asserts the value should match the given regular expression
-spec like(binary() | boolean() | number(), binary()) -> map().
regex_match(Value, Regex) ->
    pact_matchers:regex_match(Value, Regex).

%% @doc Asserts that each key of Value should match the given regular expression
-spec like(binary() | boolean() | number(), binary()) -> map().
each_key(Value, Regex) ->
    pact_matchers:each_key(Value, Regex).
