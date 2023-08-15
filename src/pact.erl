-module(pact).

-export([
    v4/2,
    interaction/2,
    verify/1,
    write/2,
    cleanup/1
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

-spec write(pact_pid(), binary()) -> ok.
write(PactPid, Path) ->
    pact_consumer:write_interaction(PactPid, Path).

%% @doc Stops pact server
-spec cleanup(pact_pid()) -> ok.
cleanup(PactPid) ->
    pact_consumer:cleanup(PactPid).
