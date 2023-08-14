-module(http_consumer).


-export([
    v4/2,
    interaction/2,
    cleanup_interaction/1
]).


-type pact_ref() :: integer().
-type pact_interaction_ref() :: integer().
-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type pact_mock_server_port() :: integer().


-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    Result = http_pact_handler:start_pact(Consumer, Provider),
    case Result of
        {ok, PactPid} -> PactPid;
        {error, {already_started, OldPactPid}} when is_pid(OldPactPid) -> OldPactPid
    end.


-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
interaction(PactPid, Interaction) ->
    {PactRef, InteractionRef} = init_interaction(PactPid, Interaction),
    RequestDetails = maps:get(with_request, Interaction, #{}),
    ok = request_builder:insert_request_details(InteractionRef, RequestDetails),
    ResponseDetails = maps:get(will_respond_with, Interaction ,#{}),
    ok = response_builder:insert_response_details(InteractionRef, ResponseDetails),
    http_mock_server:start_mock_server(PactPid, PactRef, <<"127.0.0.1">>, 0, <<"http">>).


-spec cleanup_interaction(pact_pid()) -> ok.
cleanup_interaction(PactPid) ->
    PactRef = http_pact_handler:get_pact_ref(PactPid),
    MockServerPort = http_pact_handler:get_mock_server_port(PactPid),
    ok = pactffi_nif:cleanup_mock_server(MockServerPort),
    pactffi_nif:free_pact_handle(PactRef).


%% Internal functions

-spec init_interaction(pact_pid(), pact_interaction_details()) -> {pact_ref(), pact_interaction_ref()}.
init_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = http_pact_handler:get_consumer_producer(PactPid),
    PactRef = pactffi_nif:new_pact(Consumer, Producer),
    ok = http_pact_handler:set_pact_ref(PactPid, PactRef),
    GivenState = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pactffi_nif:new_interaction(PactRef, GivenState),
    ok = http_pact_handler:create_interaction(PactPid, InteractionRef, Interaction),
    {PactRef, InteractionRef}.
