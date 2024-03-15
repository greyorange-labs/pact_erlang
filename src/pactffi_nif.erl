-module(pactffi_nif).

%% Non-NIF functions
-export([
    get_mismatches/1
]).

%% Pact NIF functions
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
    given/2,
    given_with_params/3,
    new_msg_interaction/2,
    msg_given/2,
    msg_given_with_param/3,
    msg_with_contents/3,
    reify_message/1,
    get_reified_message/1,
    new_verifier/2,
    verifier_set_provider_info/6,
    verifier_add_provider_transport/5,
    verifier_set_provider_state/4,
    verifier_set_publish_options/3,
    verifier_add_file_source/2,
    verifier_add_broker/8,
    verifier_execute/1,
    verifier_shutdown/1
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
    given/2,
    given_with_params/3,
    new_msg_interaction/2,
    msg_given/2,
    msg_given_with_param/3,
    msg_with_contents/3,
    reify_message/1,
    new_verifier/2,
    verifier_set_provider_info/6,
    verifier_add_provider_transport/5,
    verifier_set_provider_state/4,
    verifier_set_publish_options/3,
    verifier_add_file_source/2,
    verifier_add_broker/8,
    verifier_execute/1,
    verifier_shutdown/1
]).
-on_load(init/0).

%% Non-NIF functions (mostly just wrappers around NIF functions)
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

%% Non-NIF functions (mostly just wrappers around NIF functions)
%% @doc Returns reified message contents
-spec get_reified_message(integer()) -> [] | thoas:json_term().
get_reified_message(InteractionHandle) ->
    case pactffi_nif:reify_message(InteractionHandle) of
        {error, _} ->
            [];
        {ok, Json} ->
            {ok, ReifiedMessage} = thoas:decode(Json),
            ReifiedMessage
    end.

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

given_with_params(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

new_msg_interaction(_, _) ->
    erlang:nif_error("NIF library not loaded").

msg_given(_, _) ->
    erlang:nif_error("NIF library not loaded").

msg_given_with_param(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

msg_with_contents(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

reify_message(_) ->
    erlang:nif_error("NIF library not loaded").

new_verifier(_, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_set_provider_info(_, _, _, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_add_provider_transport(_, _, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_set_provider_state(_, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_set_publish_options(_, _, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_add_file_source(_, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_add_broker(_, _, _, _, _, _, _, _) ->
    erlang:nif_error("NIF library not loaded").

verifier_execute(_) ->
    erlang:nif_error("NIF library not loaded").

verifier_shutdown(_) ->
    erlang:nif_error("NIF library not loaded").
