-module(pact_consumer_verify).


-export([
    verify_interaction/1,
    write_interaction/2
]).


-type pact_pid() :: pid().


-spec verify_interaction(pact_pid()) -> {ok, matched} | {error, not_matched}.
verify_interaction(PactPid) ->
    MockServerPort = http_pact_handler:get_mock_server_port(PactPid),
    pact_nif_interface:verify(MockServerPort).


-spec write_interaction(pact_pid(), binary()) -> ok.
write_interaction(PactPid, Path) ->
    PactRef = http_pact_handler:get_pact_ref(PactPid),
    pact_nif_interface:write_pact_file(PactRef, Path),
    http_consumer:cleanup_interaction(PactPid).
