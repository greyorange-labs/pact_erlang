-module(pactffi_nif).

%% Pact functions
-export([
    version/0,
    logger_init/0,
    logger_attach_sink/2,
    logger_apply/0,
    log_message/3,
    new_pact/2,
    new_interaction/2,
    with_request/3,
    with_header_v2/5,
    with_body/4,
    response_status/2,
    create_mock_server_for_transport/4,
    mock_server_matched/1,
    mock_server_mismatches/1,
    log_to_file/2,
    pact_handle_write_file/3,
    cleanup_mock_server/1,
    free_pact_handle/1,
    with_query_parameter_v2/4,
    given/2
]).

% Import the NIF functions from the C library
-nifs([
    version/0,
    logger_init/0,
    logger_attach_sink/2,
    logger_apply/0,
    log_message/3,
    new_pact/2,
    new_interaction/2,
    with_request/3,
    with_header_v2/5,
    with_body/4,
    response_status/2,
    create_mock_server_for_transport/4,
    mock_server_matched/1,
    mock_server_mismatches/1,
    log_to_file/2,
    pact_handle_write_file/3,
    cleanup_mock_server/1,
    free_pact_handle/1,
    with_query_parameter_v2/4,
    given/2
]).
-on_load(init/0).

% Load the NIF library
init() ->
  % Adjust the path to the built NIF wrapper library
  Path = code:priv_dir(pact_erlang) ++ "/libpact_ffi",
  ok = erlang:load_nif(Path, 0).

% Define the Erlang functions that calls the NIF functions
version() ->
  erlang:nif_error("NIF library not loaded").

logger_init() ->
    erlang:nif_error("NIF library not loaded").

logger_attach_sink(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

logger_apply() ->
    erlang:nif_error("NIF library not loaded").

log_message(_Arg1, _Arg2, _Arg3) ->
    erlang:nif_error("NIF library not loaded").

new_pact(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

new_interaction(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

with_request(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

with_header_v2(_, _, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

with_body(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

response_status(_, _) ->
    erlang:nif_error("NIF library not loaded").

create_mock_server_for_transport(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

mock_server_matched(_) ->
    erlang:nif_error("NIF library not loaded").

mock_server_mismatches(_) ->
    erlang:nif_error("NIF library not loaded").

log_to_file(_, _) ->
    erlang:nif_error("NIF library not loaded").

pact_handle_write_file(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

cleanup_mock_server(_) ->
    erlang:nif_error("NIF library not loaded").

free_pact_handle(_) ->
    erlang:nif_error("NIF library not loaded").

with_query_parameter_v2(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

given(_, _) ->
    erlang:nif_error("NIF library not loaded").
