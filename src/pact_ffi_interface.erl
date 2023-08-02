-module(pact_ffi_interface).

%% Pact functions
-export([
    pactffi_version/0,
    pactffi_logger_init/0,
    pactffi_logger_attach_sink/2,
    pactffi_logger_apply/0,
    pactffi_log_message/3,
    create_new_pact/2,
    create_new_interaction/2,
    with_request/3,
    with_request_header/4,
    with_request_body/3,
    with_response_status/2,
    with_response_header/4,
    with_response_body/3,
    create_mock_server_for_transport/4,
    verify/1,
    get_mismatches/1,
    pactffi_log_to_file/2,
    write_pact_file/2,
    cleanup_mock_server/1,
    cleanup_pact/1,
    with_query_parameter/4,
    given/2
]).


pactffi_version() ->
    pact_ffi_nif:erl_pactffi_version().

pactffi_logger_init() ->
    pact_ffi_nif:erl_pactffi_logger_init().

pactffi_logger_attach_sink(LogPath, LogLevel) ->
    pact_ffi_nif:erl_pactffi_logger_attach_sink(LogPath, LogLevel).

pactffi_logger_apply() ->
    pact_ffi_nif:erl_pactffi_logger_apply().

pactffi_log_message(Source, LogLevel, Message) ->
    pact_ffi_nif:erl_pactffi_log_message(Source, LogLevel, Message).

create_new_pact(Consumer, Producer) ->
    pact_ffi_nif:erl_pactffi_new_pact(Consumer, Producer).

create_new_interaction(PactRef, InteractionDescription) ->
    pact_ffi_nif:erl_pactffi_new_interaction(PactRef, InteractionDescription).

with_request(InteractionRef, ReqMethod, ReqPath) ->
    pact_ffi_nif:erl_pactffi_with_request(InteractionRef, ReqMethod, ReqPath).

with_request_header(InteractionRef, HeaderKey, Index, HeaderValue) ->
    pact_ffi_nif:erl_pactffi_with_header_v2(InteractionRef, 0, HeaderKey, Index, HeaderValue).

with_request_body(InteractionRef, ContentType, Json) ->
    pact_ffi_nif:erl_pactffi_with_body(InteractionRef, 0, ContentType, Json).

with_response_status(InteractionRef, ResponseCode) ->
    pact_ffi_nif:erl_pactffi_response_status(InteractionRef, ResponseCode).

with_response_header(InteractionRef, HeaderKey, Index, HeaderValue) ->
    pact_ffi_nif:erl_pactffi_with_header_v2(InteractionRef, 1, HeaderKey, Index, HeaderValue).

with_response_body(InteractionRef, ContentType, Json) ->
    pact_ffi_nif:erl_pactffi_with_body(InteractionRef, 1, ContentType, Json).

create_mock_server_for_transport(PactRef, Address, Port, TransportType) ->
    pact_ffi_nif:erl_pactffi_create_mock_server_for_transport(PactRef, Address, Port, TransportType).

verify(MockServerPort) ->
    pact_ffi_nif:erl_pactffi_mock_server_matched(MockServerPort).

get_mismatches(MockServerPort) ->
    case pact_ffi_nif:erl_pactffi_mock_server_mismatches(MockServerPort) of
        undefined ->
            [];
        Json ->
            {ok, Mismatches} = thoas:decode(Json),
            Mismatches
    end.

pactffi_log_to_file(FilePath, LogLevel) ->
    pact_ffi_nif:erl_pactffi_log_to_file(FilePath, LogLevel).

write_pact_file(PactRef, PactDir) ->
    pact_ffi_nif:erl_pactffi_pact_handle_write_file(PactRef, PactDir, 0).

cleanup_mock_server(MockServerPort) ->
    pact_ffi_nif:erl_pactffi_cleanup_mock_server(MockServerPort).

cleanup_pact(PactRef) ->
    pact_ffi_nif:erl_pactffi_free_pact_handle(PactRef).

with_query_parameter(InteractionRef, Name, Index, Value) ->
    pact_ffi_nif:erl_pactffi_with_query_parameter_v2(InteractionRef, Name, Index, Value).

given(InteractionRef, State) ->
    pact_ffi_nif:erl_pactffi_given(InteractionRef, State).
