-module(pact_nif_interface).

%% Pact nif wrapper functions
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

%% @doc Returns pact ffi version
-spec pactffi_version() -> list().
pactffi_version() ->
    pactffi_nif:version().

%% @doc Starts pact ffi logging process
-spec pactffi_logger_init() -> ok.
pactffi_logger_init() ->
    pactffi_nif:logger_init().

%% @doc Attaches logger sink to specified log path and log level
-spec pactffi_logger_attach_sink(binary(), integer()) -> ok.
pactffi_logger_attach_sink(LogPath, LogLevel) ->
    pactffi_nif:logger_attach_sink(LogPath, LogLevel).

%% @doc Applies the logger configuration
-spec pactffi_logger_apply() -> ok.
pactffi_logger_apply() ->
    pactffi_nif:logger_apply().

%% @doc Logs pact ffi message
-spec pactffi_log_message(binary(), binary(), binary()) -> ok.
pactffi_log_message(Source, LogLevel, Message) ->
    pactffi_nif:log_message(Source, LogLevel, Message).

%% @doc Creates new pact internally with given consumer and producer
%% Returns the opaque pointer to this pact
-spec create_new_pact(binary(), binary()) -> integer().
create_new_pact(Consumer, Producer) ->
    pactffi_nif:new_pact(Consumer, Producer).

%% @doc Creates new interaction internally for a pact
%% Returns the opaque pointer to this interaction
-spec create_new_interaction(integer(), binary()) -> integer().
create_new_interaction(PactRef, InteractionDescription) ->
    pactffi_nif:new_interaction(PactRef, InteractionDescription).

%% @doc Adds request method and path to the interaction
-spec with_request(integer(), binary(), binary()) ->
    {ok, request_path_added} | {error, cannot_add_request_path}.
with_request(InteractionRef, ReqMethod, ReqPath) ->
    pactffi_nif:with_request(InteractionRef, ReqMethod, ReqPath).

%% @doc Adds request header to the interaction
-spec with_request_header(integer(), binary(), integer(), binary()) ->
    {ok, header_added} | {error, cannot_add_header}.
with_request_header(InteractionRef, HeaderKey, Index, HeaderValue) ->
    pactffi_nif:with_header_v2(InteractionRef, 0, HeaderKey, Index, HeaderValue).

%% @doc Adds request body to the interaction
-spec with_request_body(integer(), binary(), binary()) ->
    {ok, body_added} | {error, cannot_add_body}.
with_request_body(InteractionRef, ContentType, Json) ->
    pactffi_nif:with_body(InteractionRef, 0, ContentType, Json).

%% @doc Adds response status to the interaction
-spec with_response_status(integer(), integer()) ->
    {ok, response_status_added} | {error, cannot_add_response_status}.
with_response_status(InteractionRef, ResponseCode) ->
    pactffi_nif:response_status(InteractionRef, ResponseCode).

%% @doc Adds response header to the interaction
-spec with_response_header(integer(), binary(), integer(), binary()) ->
    {ok, header_added} | {error, cannot_add_header}.
with_response_header(InteractionRef, HeaderKey, Index, HeaderValue) ->
    pactffi_nif:with_header_v2(InteractionRef, 1, HeaderKey, Index, HeaderValue).

%% @doc Adds response body to the interaction
-spec with_response_body(integer(), binary(), binary()) ->
    {ok, body_added} | {error, cannot_add_body}.
with_response_body(InteractionRef, ContentType, Json) ->
    pactffi_nif:with_body(InteractionRef, 1, ContentType, Json).

%% @doc Starts mock server with given pact reference
-spec create_mock_server_for_transport(integer(), binary(), integer(), binary()) -> integer().
create_mock_server_for_transport(PactRef, Address, Port, TransportType) ->
    pactffi_nif:create_mock_server_for_transport(PactRef, Address, Port, TransportType).

%% @doc Verifies all the interactions given when starting a mock server
%% Returns {ok, matched} iff all the interactions
%% were successfully executed
-spec verify(integer()) -> {ok, matched} | {error, not_matched}.
verify(MockServerPort) ->
    pactffi_nif:mock_server_matched(MockServerPort).

%% @doc Returns all the interaction mismatches
-spec get_mismatches(integer()) -> [] | thoas:json_term().
get_mismatches(MockServerPort) ->
    case pactffi_nif:mock_server_mismatches(MockServerPort) of
        undefined ->
            [];
        Json ->
            {ok, Mismatches} = thoas:decode(Json),
            Mismatches
    end.

%% @doc Directs all pact ffi logging to a file
-spec pactffi_log_to_file(binary(), integer()) -> ok.
pactffi_log_to_file(FilePath, LogLevel) ->
    pactffi_nif:log_to_file(FilePath, LogLevel).

%% @doc Writes pact file with a given pact ref
-spec write_pact_file(integer(), binary()) -> ok.
write_pact_file(PactRef, PactDir) ->
    pactffi_nif:pact_handle_write_file(PactRef, PactDir, 0),
    ok.

%% @doc Cleanup mock server
-spec cleanup_mock_server(integer()) -> ok.
cleanup_mock_server(MockServerPort) ->
    pactffi_nif:cleanup_mock_server(MockServerPort).

%% @doc Cleanup internal pact object and its references
-spec cleanup_pact(integer()) -> ok.
cleanup_pact(PactRef) ->
    pactffi_nif:free_pact_handle(PactRef).

%% @doc Adds query param to the interaction
-spec with_query_parameter(integer(), binary(), integer(), binary()) ->
    {ok, query_param_added} | {error, cannot_add_query_param}.
with_query_parameter(InteractionRef, Name, Index, Value) ->
    pactffi_nif:with_query_parameter_v2(InteractionRef, Name, Index, Value).

%% @doc Adds provider state to the interaction
-spec given(integer(), binary()) -> 
    {ok, provider_state_added} | {error, cannot_add_provider_state}.
given(InteractionRef, State) ->
    pactffi_nif:given(InteractionRef, State).
