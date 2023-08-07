-module(pact).

-export([
    v4/2, create_interaction/2,
    verify_interaction/1, write_pact_file/2,
    verify_interaction_and_write_pact/2,
    cleanup_pact/1
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
    Result = pact_handler:start_pact(Consumer, Provider),
    case Result of
        {ok, PactPid} -> PactPid;
        {error, {already_started, OldPactPid}} -> OldPactPid
    end.

%% @doc Creates a mock server with the given interaction details
%% Returns its port for running pact consumer tests
-spec create_interaction(pact_pid(), pact_interaction_details()) ->
    {ok, pact_mock_server_port()}.
create_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = pact_handler:get_consumer_producer(PactPid),
    PactRef = pact_nif_interface:create_new_pact(Consumer, Producer),
    ok = pact_handler:set_pact_ref(PactPid, PactRef),
    GivenState = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pact_nif_interface:create_new_interaction(PactRef, GivenState),
    ok = pact_handler:create_interaction(PactPid, InteractionRef, Interaction),
    RequestDetails = maps:get(with_request, Interaction, #{}),
    ok = insert_request_details(InteractionRef, RequestDetails),
    ResponseDetails = maps:get(will_respond_with, Interaction ,#{}),
    ok = insert_response_details(InteractionRef, ResponseDetails),
    MockServerPort = pact_nif_interface:create_mock_server_for_transport(
        PactRef, <<"127.0.0.1">>, 0, <<"http">>
    ),
    ok = pact_handler:set_mock_server_port(PactPid, MockServerPort),
    {ok, MockServerPort}.

%% @doc Verifies interactions
-spec verify_interaction(pact_pid()) -> {ok, matched} | {error, not_matched}.
verify_interaction(PactPid) ->
    MockServerPort = pact_handler:get_mock_server_port(PactPid),
    pact_nif_interface:verify(MockServerPort).

%% @doc Verifies and writes pact file
-spec verify_interaction_and_write_pact(pact_pid(), binary()) -> ok.
verify_interaction_and_write_pact(PactPid, Path) ->
    {ok, matched} = verify_interaction(PactPid),
    write_pact_file(PactPid, Path).

%% @doc Writes pact file and also finally cleanups
-spec write_pact_file(pact_pid(), binary()) -> ok.
write_pact_file(PactPid, Path) ->
    PactRef = pact_handler:get_pact_ref(PactPid),
    pact_nif_interface:write_pact_file(PactRef, Path),
    cleanup_internal(PactPid).

%% @doc Stops pact server
-spec cleanup_pact(pact_pid()) -> ok.
cleanup_pact(PactPid) ->
    pact_handler:stop(PactPid).


%% Internal functions

cleanup_internal(PactPid) ->
    PactRef = pact_handler:get_pact_ref(PactPid),
    MockServerPort = pact_handler:get_mock_server_port(PactPid),
    ok = pact_nif_interface:cleanup_mock_server(MockServerPort),
    pact_nif_interface:cleanup_pact(PactRef).

insert_request_details(InteractionRef, RequestDetails) ->
    ReqMethod = maps:get(method, RequestDetails),
    ReqPath = maps:get(path, RequestDetails),
    pact_nif_interface:with_request(InteractionRef, ReqMethod, ReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, undefined),
    case ReqHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_nif_interface:with_request_header(InteractionRef, Key, 0, Value)
                end,
                ok,
                ReqHeaders
            )
    end,
    ReqBody = maps:get(body, RequestDetails, undefined),
    case ReqBody of
        undefined -> ok;
        _ ->
            Body = maps:get(body, ReqBody, <<"">>),
            ContentType = maps:get(content_type, ReqBody, <<"">>),
            pact_nif_interface:with_request_body(InteractionRef, ContentType, Body)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_nif_interface:with_query_parameter(InteractionRef, Key, 0, Value)
                end,
                ok,
                ReqQueryParams
            )
    end,
    ok.

insert_response_details(InteractionRef, ResponseDetails) ->
    ResponseStatusCode = maps:get(status, ResponseDetails, undefined),
    case ResponseStatusCode of
        undefined -> ok;
        _ ->
            pact_nif_interface:with_response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, undefined),
    case ResHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_nif_interface:with_response_header(InteractionRef, Key, 0, Value)
                end,
                ok,
                ResHeaders
            )
    end,
    ResBody = maps:get(body, ResponseDetails, undefined),
    case ResBody of
        undefined -> ok;
        _ ->
            Body = maps:get(body, ResBody, <<"">>),
            ContentType = maps:get(content_type, ResBody, <<"">>),
            pact_nif_interface:with_response_body(InteractionRef, ContentType, Body)
    end,
    ok.
