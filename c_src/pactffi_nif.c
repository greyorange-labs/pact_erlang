/* niftest.c */
#include <erl_nif.h>
#include <pact.h>
#include <string.h>

static char *convert_erl_binary_to_c_string(ErlNifEnv *env, ERL_NIF_TERM binary_term)
{
    ErlNifBinary binary;
    char *str;

    // Get the Erlang binary term
    enif_inspect_binary(env, binary_term, &binary);

    // Allocate memory for the string
    str = (char *)enif_alloc(binary.size + 1);

    // Copy the binary data into the string
    memcpy(str, binary.data, binary.size);

    // Add a null-terminating character
    str[binary.size] = '\0';

    return str;
}

static int convert_erl_int_to_c_int(ErlNifEnv *env, ERL_NIF_TERM int_term)
{
    int c_int;
    enif_get_int(env, int_term, &c_int);
    return c_int;
}

static ERL_NIF_TERM version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, pactffi_version(), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM logger_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    pactffi_logger_init();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM logger_attach_sink(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_binary(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    char *logPath = convert_erl_binary_to_c_string(env, argv[0]);
    if (!enif_is_number(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    int log_lvl = convert_erl_int_to_c_int(env, argv[1]);
    pactffi_logger_attach_sink(logPath, log_lvl);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM logger_apply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    pactffi_logger_apply();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM log_message(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_binary(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    char *source = convert_erl_binary_to_c_string(env, argv[0]);
    char *log_level = convert_erl_binary_to_c_string(env, argv[1]);
    char *message = convert_erl_binary_to_c_string(env, argv[2]);
    pactffi_log_message(source, log_level, message);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM new_pact(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_binary(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *consumer = convert_erl_binary_to_c_string(env, argv[0]);
    char *producer = convert_erl_binary_to_c_string(env, argv[1]);
    PactHandle pacthandle;
    pacthandle = pactffi_new_pact(consumer, producer);

    return enif_make_int(env, pacthandle);
}

static ERL_NIF_TERM new_interaction(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int pact_ref = convert_erl_int_to_c_int(env, argv[0]);
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *interaction_desc = convert_erl_binary_to_c_string(env, argv[1]);
    PactHandle pacthandle = pact_ref;
    InteractionHandle interactionhandle = pactffi_new_interaction(pacthandle, interaction_desc);

    return enif_make_int(env, interactionhandle);
}

static ERL_NIF_TERM given(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *provider_state = convert_erl_binary_to_c_string(env, argv[1]);
    InteractionHandle interactionhandle = interaction_ref;
    if(pactffi_given(interactionhandle, provider_state))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "provider_state_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_provider_state"));
    }
}

static ERL_NIF_TERM given_with_params(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *provider_state = convert_erl_binary_to_c_string(env, argv[1]);
    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    char *params = convert_erl_binary_to_c_string(env, argv[2]);
    InteractionHandle interactionhandle = interaction_ref;
    if(pactffi_given_with_params(interactionhandle, provider_state, params))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "provider_state_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_provider_state"));
    }
}

static ERL_NIF_TERM with_request(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    InteractionHandle interactH = interaction_ref;

    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    char *request_method = convert_erl_binary_to_c_string(env, argv[1]);
    char *request_path = convert_erl_binary_to_c_string(env, argv[2]);

    if (pactffi_with_request(interactH, request_method, request_path))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "request_path_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_request_path"));
    }
}

static ERL_NIF_TERM with_header_v2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    InteractionHandle interactH = interaction_ref;
    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    char *header_key = convert_erl_binary_to_c_string(env, argv[2]);

    if (!enif_is_number(env, argv[3]))
    {
        return enif_make_badarg(env);
    }
    int header_value_index = convert_erl_int_to_c_int(env, argv[3]);

    if (!enif_is_binary(env, argv[4]))
    {
        return enif_make_badarg(env);
    }
    char *header_value = convert_erl_binary_to_c_string(env, argv[4]);

    if (!enif_is_number(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    int interactionPart = convert_erl_int_to_c_int(env, argv[1]);

    if (pactffi_with_header_v2(interactH, interactionPart, header_key, header_value_index, header_value))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "header_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_header"));
    }
}

static ERL_NIF_TERM with_body(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    InteractionHandle interactH = interaction_ref;

    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[3]))
    {
        return enif_make_badarg(env);
    }
    char *content_type = convert_erl_binary_to_c_string(env, argv[2]);
    char *body_json_string = convert_erl_binary_to_c_string(env, argv[3]);

    if (!enif_is_number(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    int interactionPart = convert_erl_int_to_c_int(env, argv[1]);

    if (pactffi_with_body(interactH, interactionPart, content_type, body_json_string))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "body_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_body"));
    }
}

static ERL_NIF_TERM response_status(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    InteractionHandle interactH = interaction_ref;
    if (!enif_is_number(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    int response_code = convert_erl_int_to_c_int(env, argv[1]);
    if (pactffi_response_status(interactH, response_code))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "response_status_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_response_status"));
    }
}

static ERL_NIF_TERM create_mock_server_for_transport(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int pact_ref = convert_erl_int_to_c_int(env, argv[0]);
    PactHandle pactH = pact_ref;

    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *address = convert_erl_binary_to_c_string(env, argv[1]);

    if (!enif_is_number(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    int given_port = convert_erl_int_to_c_int(env, argv[2]);

    if (!enif_is_binary(env, argv[3]))
    {
        return enif_make_badarg(env);
    }
    char *transport_type = convert_erl_binary_to_c_string(env, argv[3]);
    int return_port = pactffi_create_mock_server_for_transport(pactH, address, given_port, transport_type, NULL);

    return enif_make_int(env, return_port);
}

static ERL_NIF_TERM mock_server_matched(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int mock_server_port = convert_erl_int_to_c_int(env, argv[0]);
    if (pactffi_mock_server_matched(mock_server_port))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "matched"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "not_matched"));
    }
}

static ERL_NIF_TERM mock_server_mismatches(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int mock_server_port = convert_erl_int_to_c_int(env, argv[0]);
    char *mismatches = pactffi_mock_server_mismatches(mock_server_port);
    if (mismatches != NULL)
    {
        return enif_make_string(env, mismatches, ERL_NIF_LATIN1);
    }
    else
    {
        return enif_make_atom(env, "undefined");
    }
}

static ERL_NIF_TERM pact_handle_write_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int pact_ref = convert_erl_int_to_c_int(env, argv[0]);
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *pact_dir = convert_erl_binary_to_c_string(env, argv[1]);
    if (!enif_is_number(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    int overwrite_value = convert_erl_int_to_c_int(env, argv[2]);
    bool overwrite;
    if (overwrite_value == 0)
    {
        overwrite = false;
    }
    else
    {
        overwrite = true;
    }

    return enif_make_int(env, pactffi_pact_handle_write_file(pact_ref, pact_dir, overwrite));
}

static ERL_NIF_TERM log_to_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_binary(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    char *file_path = convert_erl_binary_to_c_string(env, argv[0]);
    if (!enif_is_number(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    int log_lvl = convert_erl_int_to_c_int(env, argv[1]);
    pactffi_log_to_file(file_path, log_lvl);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM cleanup_mock_server(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int mock_server_port = convert_erl_int_to_c_int(env, argv[0]);
    pactffi_cleanup_mock_server(mock_server_port);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM free_pact_handle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    PactHandle pactHandle = convert_erl_int_to_c_int(env, argv[0]);
    pactffi_free_pact_handle(pactHandle);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM with_query_parameter_v2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    InteractionHandle interactH = interaction_ref;
    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    char *name = convert_erl_binary_to_c_string(env, argv[1]);

    if (!enif_is_number(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    int index = convert_erl_int_to_c_int(env, argv[2]);

    if (!enif_is_binary(env, argv[3]))
    {
        return enif_make_badarg(env);
    }
    char *value = convert_erl_binary_to_c_string(env, argv[3]);

    if (pactffi_with_query_parameter_v2(interactH, name, index, value))
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "query_param_added"));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_add_query_param"));
    }
}

static ErlNifFunc nif_funcs[] =
    {
        {"version", 0, version},
        {"logger_init", 0, logger_init},
        {"logger_attach_sink", 2, logger_attach_sink},
        {"logger_apply", 0, logger_apply},
        {"log_message", 3, log_message},
        {"new_pact", 2, new_pact},
        {"new_interaction", 2, new_interaction},
        {"with_request", 3, with_request},
        {"with_header_v2", 5, with_header_v2},
        {"with_body", 4, with_body},
        {"response_status", 2, response_status},
        {"create_mock_server_for_transport", 4, create_mock_server_for_transport},
        {"mock_server_matched", 1, mock_server_matched},
        {"mock_server_mismatches", 1, mock_server_mismatches},
        {"log_to_file", 2, log_to_file},
        {"pact_handle_write_file", 3, pact_handle_write_file},
        {"cleanup_mock_server", 1, cleanup_mock_server},
        {"free_pact_handle", 1, free_pact_handle},
        {"with_query_parameter_v2", 4, with_query_parameter_v2},
        {"given", 2, given},
        {"given_with_params", 3, given_with_params}
    };

ERL_NIF_INIT(pactffi_nif, nif_funcs, NULL, NULL, NULL, NULL)
