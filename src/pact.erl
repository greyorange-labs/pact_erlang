-module(pact).

-export([
    v4/2, create_interaction/2,
    verify_interaction/1, write_pact_file/2,
    verify_interaction_and_write_pact/2,
    cleanup_pact/1
]).

v4(Consumer, Producer) ->
    Result = pact_handler:start_pact(Consumer, Producer),
    case Result of
        {ok, PactPid} -> PactPid;
        {error, {already_started, OldPactPid}} -> OldPactPid
    end.

create_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = pact_handler:get_consumer_producer(PactPid),
    PactRef = pact_ffi_interface:create_new_pact(Consumer, Producer),
    ok = pact_handler:set_pact_ref(PactPid, PactRef),
    GivenState = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pact_ffi_interface:create_new_interaction(PactRef, GivenState),
    ok = pact_handler:create_interaction(PactPid, InteractionRef, Interaction),
    RequestDetails = maps:get(with_request, Interaction, #{}),
    ok = insert_request_details(InteractionRef, RequestDetails),
    ResponseDetails = maps:get(will_respond_with, Interaction ,#{}),
    ok = insert_response_details(InteractionRef, ResponseDetails),
    MockServerPort = pact_ffi_interface:create_mock_server_for_transport(
        PactRef, <<"127.0.0.1">>, 0, <<"http">>
    ),
    ok = pact_handler:set_mock_server_port(PactPid, MockServerPort),
    {ok, MockServerPort}.

verify_interaction(PactPid) ->
    MockServerPort = pact_handler:get_mock_server_port(PactPid),
    {ok, matched} = pact_ffi_interface:verify(MockServerPort).

verify_interaction_and_write_pact(PactPid, Path) ->
    verify_interaction(PactPid),
    write_pact_file(PactPid, Path).

write_pact_file(PactPid, Path) ->
    PactRef = pact_handler:get_pact_ref(PactPid),
    pact_ffi_interface:write_pact_file(PactRef, Path),
    cleanup_internal(PactPid).

cleanup_internal(PactPid) ->
    PactRef = pact_handler:get_pact_ref(PactPid),
    MockServerPort = pact_handler:get_mock_server_port(PactPid),
    ok = pact_ffi_interface:cleanup_mock_server(MockServerPort),
    pact_ffi_interface:cleanup_pact(PactRef).

cleanup_pact(PactPid) ->
    pact_handler:stop(PactPid).

insert_request_details(InteractionRef, RequestDetails) ->
    ReqMethod = maps:get(method, RequestDetails),
    ReqPath = maps:get(path, RequestDetails),
    pact_ffi_interface:with_request(InteractionRef, ReqMethod, ReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, undefined),
    case ReqHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_interface:with_request_header(InteractionRef, Key, 0, Value)
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
            pact_ffi_interface:with_request_body(InteractionRef, ContentType, Body)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_interface:with_query_parameter(InteractionRef, Key, 0, Value)
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
            pact_ffi_interface:with_response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, undefined),
    case ResHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_interface:with_response_header(InteractionRef, Key, 0, Value)
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
            pact_ffi_interface:with_response_body(InteractionRef, ContentType, Body)
    end,
    ok.
