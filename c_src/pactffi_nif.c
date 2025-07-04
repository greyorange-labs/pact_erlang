/* niftest.c */
#include <erl_nif.h>
#include <pact.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/stat.h>

static const char *const * binary_to_char_array(ErlNifEnv* env, const ERL_NIF_TERM binary_term) {
    ErlNifBinary binary;
    
    // Extract the binary data from the Erlang term
    if (!enif_inspect_binary(env, binary_term, &binary))
        return NULL;

    // Allocate memory for an array of const char *
    const char **char_array = enif_alloc(sizeof(const char *) * (binary.size + 1)); // +1 for NULL terminator
    if (!char_array)
        return NULL;

    // Convert each byte in the binary data to a const char *
    for (int i = 0; i < binary.size; i++) {
        char *str = enif_alloc(2); // Allocate memory for a single character plus null terminator
        if (!str) {
            // Free already allocated memory
            for (int j = 0; j < i; j++) {
                enif_free((void *)char_array[j]);
            }
            enif_free(char_array);
            return NULL;
        }
        snprintf(str, 2, "%c", binary.data[i]); // Convert byte to a single-character string
        char_array[i] = str;
    }

    // Null-terminate the array
    char_array[binary.size] = NULL;

    return char_array;
}

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

static uint8_t* convertCharToUint8(const char* input) {
    // Calculate the length of the string
    size_t length = strlen(input);

    // Allocate memory for the new uint8_t array
    uint8_t* output = (uint8_t*)malloc((length + 1) * sizeof(uint8_t)); // +1 for null terminator
    
    // Copy the data from char* to uint8_t*
    for (size_t i = 0; i < length; i++) {
        output[i] = (uint8_t)input[i];
    }

    // Null-terminate the uint8_t array
    output[length] = '\0';

    return output;
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
    int result = pactffi_logger_attach_sink(logPath, log_lvl);
    return enif_make_int(env, result);
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

static ERL_NIF_TERM new_msg_interaction(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
    InteractionHandle interactionhandle = pactffi_new_message_interaction(pacthandle, interaction_desc);

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

static ERL_NIF_TERM msg_given(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
    MessageHandle interactionhandle = interaction_ref;
    pactffi_message_given(interactionhandle, provider_state);
    
    return enif_make_atom(env, "ok"); 
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

static ERL_NIF_TERM msg_given_with_param(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
    char *name = convert_erl_binary_to_c_string(env, argv[2]);
    if (!enif_is_binary(env, argv[3]))
    {
        return enif_make_badarg(env);
    }
    char *value = convert_erl_binary_to_c_string(env, argv[3]);
    MessageHandle interactionhandle = interaction_ref;
    pactffi_message_given_with_param(interactionhandle, provider_state, name, value);
    
    return enif_make_atom(env, "ok");
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

static ERL_NIF_TERM msg_with_contents(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    MessageHandle interactH = interaction_ref;

    if (!enif_is_binary(env, argv[1]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[2]))
    {
        return enif_make_badarg(env);
    }
    char *content_type = convert_erl_binary_to_c_string(env, argv[1]);
    uint8_t *body_json_string = convertCharToUint8(convert_erl_binary_to_c_string(env, argv[2]));

    pactffi_message_with_contents(interactH, content_type, body_json_string, -1);

    free(body_json_string);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM reify_message(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    int interaction_ref = convert_erl_int_to_c_int(env, argv[0]);
    MessageHandle interactH = interaction_ref;

    const char *reified_message = pactffi_message_reify(interactH);

    if (reified_message != NULL)
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_string(env, reified_message, ERL_NIF_LATIN1));
    }
    else
    {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cannot_reify_message"));
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

// Provider verifier functions

static ERL_NIF_TERM verify_via_file_direct(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // Extract all parameters
    if (!enif_is_binary(env, argv[0])) return enif_make_badarg(env);
    char *name = convert_erl_binary_to_c_string(env, argv[0]);
    
    if (!enif_is_binary(env, argv[1])) return enif_make_badarg(env);
    char *scheme = convert_erl_binary_to_c_string(env, argv[1]);
    
    if (!enif_is_binary(env, argv[2])) return enif_make_badarg(env);
    char *host = convert_erl_binary_to_c_string(env, argv[2]);
    
    if (!enif_is_number(env, argv[3])) return enif_make_badarg(env);
    int port = convert_erl_int_to_c_int(env, argv[3]);
    
    if (!enif_is_binary(env, argv[4])) return enif_make_badarg(env);
    char *path = convert_erl_binary_to_c_string(env, argv[4]);
    
    if (!enif_is_binary(env, argv[5])) return enif_make_badarg(env);
    char *version = convert_erl_binary_to_c_string(env, argv[5]);
    
    if (!enif_is_binary(env, argv[6])) return enif_make_badarg(env);
    char *branch = convert_erl_binary_to_c_string(env, argv[6]);
    
    if (!enif_is_binary(env, argv[7])) return enif_make_badarg(env);
    char *file_path = convert_erl_binary_to_c_string(env, argv[7]);
    
    if (!enif_is_binary(env, argv[8])) return enif_make_badarg(env);
    char *protocol = convert_erl_binary_to_c_string(env, argv[8]);
    
    if (!enif_is_binary(env, argv[9])) return enif_make_badarg(env);
    char *state_path = convert_erl_binary_to_c_string(env, argv[9]);

    // Create verifier handle and configure it
    struct VerifierHandle *verifierhandle = pactffi_verifier_new_for_application(name, version);
    pactffi_verifier_set_no_pacts_is_error(verifierhandle, 0);
    pactffi_verifier_set_provider_info(verifierhandle, name, scheme, host, port, path);
    pactffi_verifier_add_provider_transport(verifierhandle, protocol, port, path, scheme);
    
    if (state_path[0] != '\0') {
        pactffi_verifier_set_provider_state(verifierhandle, state_path, 0, 1);
    }
    
    pactffi_verifier_set_verification_options(verifierhandle, 0, 5000);
    pactffi_verifier_set_publish_options(verifierhandle, version, NULL, NULL, -1, branch);
    pactffi_verifier_add_directory_source(verifierhandle, file_path);
    
    setenv("PACT_DO_NOT_TRACK", "true", 1);
    
    // Execute verification
    int output = pactffi_verifier_execute(verifierhandle);
    
    // Cleanup
    pactffi_verifier_shutdown(verifierhandle);
    
    // Free allocated strings
    enif_free(name);
    enif_free(scheme);
    enif_free(host);
    enif_free(path);
    enif_free(version);
    enif_free(branch);
    enif_free(file_path);
    enif_free(protocol);
    enif_free(state_path);

    return enif_make_int(env, output);
}

static ERL_NIF_TERM verify_via_broker_direct(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // Extract all parameters (similar to existing verify_via_broker but with proper cleanup)
    if (!enif_is_binary(env, argv[0])) return enif_make_badarg(env);
    char *name = convert_erl_binary_to_c_string(env, argv[0]);
    
    if (!enif_is_binary(env, argv[1])) return enif_make_badarg(env);
    char *scheme = convert_erl_binary_to_c_string(env, argv[1]);
    
    if (!enif_is_binary(env, argv[2])) return enif_make_badarg(env);
    char *host = convert_erl_binary_to_c_string(env, argv[2]);
    
    if (!enif_is_number(env, argv[3])) return enif_make_badarg(env);
    int port = convert_erl_int_to_c_int(env, argv[3]);
    
    if (!enif_is_binary(env, argv[4])) return enif_make_badarg(env);
    char *path = convert_erl_binary_to_c_string(env, argv[4]);
    
    if (!enif_is_binary(env, argv[5])) return enif_make_badarg(env);
    char *version = convert_erl_binary_to_c_string(env, argv[5]);
    
    if (!enif_is_binary(env, argv[6])) return enif_make_badarg(env);
    char *branch = convert_erl_binary_to_c_string(env, argv[6]);
    
    if (!enif_is_binary(env, argv[7])) return enif_make_badarg(env);
    char *broker_url = convert_erl_binary_to_c_string(env, argv[7]);
    
    if (!enif_is_binary(env, argv[8])) return enif_make_badarg(env);
    char *broker_username = convert_erl_binary_to_c_string(env, argv[8]);
    
    if (!enif_is_binary(env, argv[9])) return enif_make_badarg(env);
    char *broker_password = convert_erl_binary_to_c_string(env, argv[9]);
    
    if (!enif_is_number(env, argv[10])) return enif_make_badarg(env);
    int enable_pending = convert_erl_int_to_c_int(env, argv[10]);
    
    if (!enif_is_binary(env, argv[11])) return enif_make_badarg(env);
    const char *const *consumer_version_selectors = binary_to_char_array(env, argv[11]);
    
    if (!enif_is_number(env, argv[12])) return enif_make_badarg(env);
    int consumer_version_selectors_len = convert_erl_int_to_c_int(env, argv[12]);
    
    if (!enif_is_binary(env, argv[13])) return enif_make_badarg(env);
    char *protocol = convert_erl_binary_to_c_string(env, argv[13]);
    
    if (!enif_is_binary(env, argv[14])) return enif_make_badarg(env);
    char *state_path = convert_erl_binary_to_c_string(env, argv[14]);

    // Create and configure verifier
    struct VerifierHandle *verifierhandle = pactffi_verifier_new_for_application(name, version);
    pactffi_verifier_set_no_pacts_is_error(verifierhandle, 0);
    pactffi_verifier_set_provider_info(verifierhandle, name, scheme, host, port, path);
    pactffi_verifier_add_provider_transport(verifierhandle, protocol, port, path, scheme);

    if (state_path[0] != '\0') {
        pactffi_verifier_set_provider_state(verifierhandle, state_path, 0, 1);
    }
    
    pactffi_verifier_set_verification_options(verifierhandle, 0, 5000);
    pactffi_verifier_set_publish_options(verifierhandle, version, NULL, NULL, -1, branch);
    pactffi_verifier_broker_source_with_selectors(verifierhandle, broker_url, broker_username, broker_password, NULL, enable_pending, NULL, NULL, -1, branch, consumer_version_selectors, consumer_version_selectors_len, NULL, -1);
    
    setenv("PACT_DO_NOT_TRACK", "true", 1);

    // Execute verification
    int verification_output = pactffi_verifier_execute(verifierhandle);

    // Cleanup
    pactffi_verifier_shutdown(verifierhandle);
    
    // Free allocated strings
    enif_free(name);
    enif_free(scheme);
    enif_free(host);
    enif_free(path);
    enif_free(version);
    enif_free(branch);
    enif_free(broker_url);
    enif_free(broker_username);
    enif_free(broker_password);
    enif_free(protocol);
    enif_free(state_path);

    return enif_make_int(env, verification_output);
}

static ERL_NIF_TERM verify_via_broker_external(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // Extract and serialize all arguments as strings
    char *name = convert_erl_binary_to_c_string(env, argv[0]);
    char *scheme = convert_erl_binary_to_c_string(env, argv[1]);
    char *host = convert_erl_binary_to_c_string(env, argv[2]);
    int port = convert_erl_int_to_c_int(env, argv[3]);
    char *path = convert_erl_binary_to_c_string(env, argv[4]);
    char *version = convert_erl_binary_to_c_string(env, argv[5]);
    char *branch = convert_erl_binary_to_c_string(env, argv[6]);
    char *broker_url = convert_erl_binary_to_c_string(env, argv[7]);
    char *broker_username = convert_erl_binary_to_c_string(env, argv[8]);
    char *broker_password = convert_erl_binary_to_c_string(env, argv[9]);
    int enable_pending = convert_erl_int_to_c_int(env, argv[10]);
    char *consumer_version_selectors = convert_erl_binary_to_c_string(env, argv[11]);
    int consumer_version_selectors_len = convert_erl_int_to_c_int(env, argv[12]);
    char *protocol = convert_erl_binary_to_c_string(env, argv[13]);
    char *state_path = convert_erl_binary_to_c_string(env, argv[14]);
    char *exec_path = convert_erl_binary_to_c_string(env, argv[15]);

    // Create a temp file for config in the current working directory
    char config_file_template[256];
    snprintf(config_file_template, sizeof(config_file_template), "pact_config_XXXXXX");
    int config_fd = mkstemp(config_file_template);
    if (config_fd == -1) {
        enif_free(name); enif_free(scheme); enif_free(host); enif_free(path);
        enif_free(version); enif_free(branch); enif_free(broker_url);
        enif_free(broker_username); enif_free(broker_password);
        enif_free(consumer_version_selectors); enif_free(protocol); enif_free(state_path);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "mkstemp_config_failed", ERL_NIF_LATIN1));
    }

    // Write config as key=value pairs (one per line)
    dprintf(config_fd, "PACT_NAME=%s\n", name);
    dprintf(config_fd, "PACT_SCHEME=%s\n", scheme);
    dprintf(config_fd, "PACT_HOST=%s\n", host);
    dprintf(config_fd, "PACT_PORT=%d\n", port);
    dprintf(config_fd, "PACT_PATH=%s\n", path);
    dprintf(config_fd, "PACT_VERSION=%s\n", version);
    dprintf(config_fd, "PACT_BRANCH=%s\n", branch);
    dprintf(config_fd, "PACT_BROKER_URL=%s\n", broker_url);
    dprintf(config_fd, "PACT_BROKER_USERNAME=%s\n", broker_username);
    dprintf(config_fd, "PACT_BROKER_PASSWORD=%s\n", broker_password);
    dprintf(config_fd, "PACT_ENABLE_PENDING=%d\n", enable_pending);
    dprintf(config_fd, "PACT_PROTOCOL=%s\n", protocol);
    dprintf(config_fd, "PACT_STATE_PATH=%s\n", state_path);
    dprintf(config_fd, "CONSUMER_VERSION_SELECTORS=%s\n", consumer_version_selectors);
    dprintf(config_fd, "CONSUMER_VERSION_SELECTORS_LEN=%d\n", consumer_version_selectors_len);
    close(config_fd);

    // Create a temp file for result in the current working directory
    char result_file_template[256];
    snprintf(result_file_template, sizeof(result_file_template), "pact_result_XXXXXX");
    int result_fd = mkstemp(result_file_template);
    if (result_fd == -1) {
        unlink(config_file_template);
        enif_free(name); enif_free(scheme); enif_free(host); enif_free(path);
        enif_free(version); enif_free(branch); enif_free(broker_url);
        enif_free(broker_username); enif_free(broker_password);
        enif_free(consumer_version_selectors); enif_free(protocol); enif_free(state_path);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "mkstemp_result_failed", ERL_NIF_LATIN1));
    }
    close(result_fd);

    // Pass config and result file paths via environment variables
    char *envp[3];
    size_t sz1 = strlen("PACT_CONFIG_FILE=") + strlen(config_file_template) + 1;
    size_t sz2 = strlen("PACT_RESULT_FILE=") + strlen(result_file_template) + 1;
    envp[0] = malloc(sz1);
    envp[1] = malloc(sz2);
    snprintf(envp[0], sz1, "PACT_CONFIG_FILE=%s", config_file_template);
    snprintf(envp[1], sz2, "PACT_RESULT_FILE=%s", result_file_template);
    envp[2] = NULL;

    char *const argv_exec[] = {exec_path, NULL};

    pid_t pid = fork();
    if (pid == 0) {
        execve(exec_path, argv_exec, envp);
        perror("execve failed");
        exit(127);
    } else if (pid > 0) {
        int status;
        waitpid(pid, &status, 0);
        // Read result from file
        int verification_output = -1;
        FILE *fp = fopen(result_file_template, "r");
        if (fp) {
            if (fscanf(fp, "%d", &verification_output) != 1) {
                verification_output = -1;
            }
            fclose(fp);
            unlink(result_file_template);
        }
        unlink(config_file_template);
        free(envp[0]);
        free(envp[1]);
        enif_free(name); enif_free(scheme); enif_free(host); enif_free(path);
        enif_free(version); enif_free(branch); enif_free(broker_url);
        enif_free(broker_username); enif_free(broker_password);
        enif_free(consumer_version_selectors); enif_free(protocol); enif_free(state_path);
        return enif_make_int(env, verification_output);
    } else {
        free(envp[0]);
        free(envp[1]);
        unlink(config_file_template);
        unlink(result_file_template);
        enif_free(name); enif_free(scheme); enif_free(host); enif_free(path);
        enif_free(version); enif_free(branch); enif_free(broker_url);
        enif_free(broker_username); enif_free(broker_password);
        enif_free(consumer_version_selectors); enif_free(protocol); enif_free(state_path);
        return enif_make_atom(env, "error");
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
        {"given_with_params", 3, given_with_params},
        {"new_msg_interaction", 2, new_msg_interaction},
        {"msg_given", 2, msg_given},
        {"msg_given_with_param", 4, msg_given_with_param},
        {"msg_with_contents", 3, msg_with_contents},
        {"reify_message", 1, reify_message},
        {"verify_via_file_direct", 10, verify_via_file_direct, ERL_NIF_DIRTY_JOB_IO_BOUND},
        {"verify_via_broker_direct", 15, verify_via_broker_direct, ERL_NIF_DIRTY_JOB_IO_BOUND},
        {"verify_via_broker_external", 16, verify_via_broker_external, ERL_NIF_DIRTY_JOB_IO_BOUND }
    };

ERL_NIF_INIT(pactffi_nif, nif_funcs, NULL, NULL, NULL, NULL)
