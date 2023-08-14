-module(http_mock_server).


-export([
    start_mock_server/5
]).


-type pact_ref() :: integer().
-type pact_pid() :: pid().
-type pact_mock_server_port() :: integer().


-spec start_mock_server(pact_pid(), pact_ref(), binary(), integer(), binary())
 -> {ok, pact_mock_server_port()}.
start_mock_server(PactPid, PactRef, Host, Port, InteractionPart) ->
    MockServerPort = pactffi_nif:create_mock_server_for_transport(
        PactRef, Host, Port, InteractionPart
    ),
    ok = http_pact_handler:set_mock_server_port(PactPid, MockServerPort),
    {ok, MockServerPort}.
