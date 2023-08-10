-module(pact_consumer).


-export([
    v4/2,
    interaction/2,
    cleanup/1
]).


-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type pact_mock_server_port() :: integer().


-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    http_consumer:v4(Consumer, Provider).


-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
interaction(PactPid, Interaction) ->
    http_consumer:interaction(PactPid, Interaction).


-spec cleanup(pact_pid()) -> ok.
cleanup(PactPid) ->
    http_consumer:cleanup_interaction(PactPid),
    http_pact_handler:stop(PactPid).
