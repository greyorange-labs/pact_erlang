-module(pact_ffi_nif).

%% Pact functions
-export([
    erl_pactffi_version/0,
    erl_pactffi_logger_init/0,
    erl_pactffi_logger_attach_sink/2,
    erl_pactffi_logger_apply/0,
    erl_pactffi_log_message/3,
    erl_pactffi_new_pact/2,
    erl_pactffi_new_interaction/2,
    erl_pactffi_with_request/3,
    erl_pactffi_with_header_v2/5,
    erl_pactffi_with_body/4,
    erl_pactffi_response_status/2,
    erl_pactffi_create_mock_server_for_transport/4,
    erl_pactffi_mock_server_matched/1,
    erl_pactffi_mock_server_mismatches/1,
    erl_pactffi_log_to_file/2,
    erl_pactffi_pact_handle_write_file/3,
    erl_pactffi_cleanup_mock_server/1,
    erl_pactffi_free_pact_handle/1,
    erl_pactffi_with_query_parameter_v2/4,
    erl_pactffi_given/2
]).

% Import the NIF functions from the C library
-nifs([
    erl_pactffi_version/0,
    erl_pactffi_logger_init/0,
    erl_pactffi_logger_attach_sink/2,
    erl_pactffi_logger_apply/0,
    erl_pactffi_log_message/3,
    erl_pactffi_new_pact/2,
    erl_pactffi_new_interaction/2,
    erl_pactffi_with_request/3,
    erl_pactffi_with_header_v2/5,
    erl_pactffi_with_body/4,
    erl_pactffi_response_status/2,
    erl_pactffi_create_mock_server_for_transport/4,
    erl_pactffi_mock_server_matched/1,
    erl_pactffi_mock_server_mismatches/1,
    erl_pactffi_log_to_file/2,
    erl_pactffi_pact_handle_write_file/3,
    erl_pactffi_cleanup_mock_server/1,
    erl_pactffi_free_pact_handle/1,
    erl_pactffi_with_query_parameter_v2/4,
    erl_pactffi_given/2
]).
-on_load(init/0).

% Load the NIF library
init() ->
  % Adjust the path to the built NIF wrapper library
  Path = code:priv_dir(pact_erlang) ++ "/libpact_ffi",
  ok = erlang:load_nif(Path, 0).

% Define the Erlang functions that calls the NIF functions
erl_pactffi_version() ->
  erlang:nif_error("NIF library not loaded").

erl_pactffi_logger_init() ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_logger_attach_sink(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_logger_apply() ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_log_message(_Arg1, _Arg2, _Arg3) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_new_pact(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_new_interaction(_Arg1, _Arg2) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_with_request(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_with_header_v2(_, _, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_with_body(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_response_status(_, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_create_mock_server_for_transport(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_mock_server_matched(_) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_mock_server_mismatches(_) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_log_to_file(_, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_pact_handle_write_file(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_cleanup_mock_server(_) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_free_pact_handle(_) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_with_query_parameter_v2(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

erl_pactffi_given(_, _) ->
    erlang:nif_error("NIF library not loaded").
