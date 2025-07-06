#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pact.h"

// Helper function to extract individual JSON objects from JSON array string
static char** parse_json_array_to_strings(const char *json_array, int *count) {
    if (!json_array || json_array[0] != '[') {
        *count = 0;
        return NULL;
    }
    
    // Count objects first
    int object_count = 0;
    int brace_level = 0;
    int in_string = 0;
    int escape_next = 0;
    
    for (const char *p = json_array; *p; p++) {
        if (escape_next) {
            escape_next = 0;
            continue;
        }
        if (*p == '\\') {
            escape_next = 1;
            continue;
        }
        if (*p == '"') {
            in_string = !in_string;
            continue;
        }
        if (in_string) continue;
        
        if (*p == '{') {
            if (brace_level == 0) object_count++;
            brace_level++;
        } else if (*p == '}') {
            brace_level--;
        }
    }
    
    if (object_count == 0) {
        *count = 0;
        return NULL;
    }
    
    // Allocate array for strings
    char **result = malloc(sizeof(char*) * object_count);
    if (!result) {
        *count = 0;
        return NULL;
    }
    
    // Extract each object
    int current_object = 0;
    brace_level = 0;
    in_string = 0;
    escape_next = 0;
    const char *object_start = NULL;
    
    for (const char *p = json_array; *p && current_object < object_count; p++) {
        if (escape_next) {
            escape_next = 0;
            continue;
        }
        if (*p == '\\') {
            escape_next = 1;
            continue;
        }
        if (*p == '"') {
            in_string = !in_string;
            continue;
        }
        if (in_string) continue;
        
        if (*p == '{') {
            if (brace_level == 0) {
                object_start = p;
            }
            brace_level++;
        } else if (*p == '}') {
            brace_level--;
            if (brace_level == 0 && object_start) {
                // End of object - extract it
                size_t len = p - object_start + 1;
                result[current_object] = malloc(len + 1);
                if (result[current_object]) {
                    strncpy(result[current_object], object_start, len);
                    result[current_object][len] = '\0';
                    current_object++;
                }
            }
        }
    }
    
    *count = current_object;
    return result;
}

static void free_string_array(char **array, int count) {
    if (array) {
        for (int i = 0; i < count; i++) {
            if (array[i]) free(array[i]);
        }
        free(array);
    }
}

// Usage: All arguments are read from a config file whose path is in PACT_CONFIG_FILE
int main() {
    // Read config file path from env
    const char *config_file = getenv("PACT_CONFIG_FILE");
    if (!config_file || config_file[0] == '\0') {
        fprintf(stderr, "PACT_CONFIG_FILE not set\n");
        return 1;
    }

    // Open and parse config file (key=value per line)
    FILE *cfp = fopen(config_file, "r");
    if (!cfp) {
        fprintf(stderr, "Failed to open config file: %s\n", config_file);
        return 1;
    }

    char line[1024];
    char name[256] = "", scheme[64] = "", host[256] = "", port_str[16] = "", path[256] = "", version[64] = "", branch[128] = "", broker_url[512] = "", broker_username[256] = "", broker_password[256] = "", enable_pending_str[8] = "", protocol[64] = "", state_path[256] = "", consumer_version_selectors[1024] = "", consumer_version_selectors_len_str[16] = "", publish_verification_results_str[8] = "";

    while (fgets(line, sizeof(line), cfp)) {
        char *eq = strchr(line, '=');
        if (!eq) continue;
        *eq = '\0';
        char *key = line;
        char *val = eq + 1;
        // Remove trailing newline
        char *nl = strchr(val, '\n');
        if (nl) *nl = '\0';
        if (strcmp(key, "PACT_NAME") == 0) strncpy(name, val, sizeof(name)-1);
        else if (strcmp(key, "PACT_SCHEME") == 0) strncpy(scheme, val, sizeof(scheme)-1);
        else if (strcmp(key, "PACT_HOST") == 0) strncpy(host, val, sizeof(host)-1);
        else if (strcmp(key, "PACT_PORT") == 0) strncpy(port_str, val, sizeof(port_str)-1);
        else if (strcmp(key, "PACT_PATH") == 0) strncpy(path, val, sizeof(path)-1);
        else if (strcmp(key, "PACT_VERSION") == 0) strncpy(version, val, sizeof(version)-1);
        else if (strcmp(key, "PACT_BRANCH") == 0) strncpy(branch, val, sizeof(branch)-1);
        else if (strcmp(key, "PACT_BROKER_URL") == 0) strncpy(broker_url, val, sizeof(broker_url)-1);
        else if (strcmp(key, "PACT_BROKER_USERNAME") == 0) strncpy(broker_username, val, sizeof(broker_username)-1);
        else if (strcmp(key, "PACT_BROKER_PASSWORD") == 0) strncpy(broker_password, val, sizeof(broker_password)-1);
        else if (strcmp(key, "PACT_ENABLE_PENDING") == 0) strncpy(enable_pending_str, val, sizeof(enable_pending_str)-1);
        else if (strcmp(key, "PACT_PROTOCOL") == 0) strncpy(protocol, val, sizeof(protocol)-1);
        else if (strcmp(key, "PACT_STATE_PATH") == 0) strncpy(state_path, val, sizeof(state_path)-1);
        else if (strcmp(key, "CONSUMER_VERSION_SELECTORS") == 0) strncpy(consumer_version_selectors, val, sizeof(consumer_version_selectors)-1);
        else if (strcmp(key, "CONSUMER_VERSION_SELECTORS_LEN") == 0) strncpy(consumer_version_selectors_len_str, val, sizeof(consumer_version_selectors_len_str)-1);
        else if (strcmp(key, "PACT_PUBLISH_VERIFICATION_RESULTS") == 0) strncpy(publish_verification_results_str, val, sizeof(publish_verification_results_str)-1);
    }
    fclose(cfp);

    printf("PACT_NAME=%s\n", name);
    printf("PACT_SCHEME=%s\n", scheme);
    printf("PACT_HOST=%s\n", host);
    printf("PACT_PORT=%s\n", port_str);
    printf("PACT_PATH=%s\n", path);
    printf("PACT_VERSION=%s\n", version);
    printf("PACT_BRANCH=%s\n", branch);
    printf("PACT_BROKER_URL=%s\n", broker_url);
    printf("PACT_BROKER_USERNAME=%s\n", broker_username);
    printf("PACT_BROKER_PASSWORD=%s\n", broker_password);
    printf("PACT_ENABLE_PENDING=%s\n", enable_pending_str);
    printf("PACT_PROTOCOL=%s\n", protocol);
    printf("PACT_STATE_PATH=%s\n", state_path);
    printf("CONSUMER_VERSION_SELECTORS=%s\n", consumer_version_selectors);
    printf("CONSUMER_VERSION_SELECTORS_LEN=%s\n", consumer_version_selectors_len_str);
    printf("PACT_PUBLISH_VERIFICATION_RESULTS=%s\n", publish_verification_results_str);

    if (!name[0] || !scheme[0] || !host[0] || !port_str[0] || !path[0] || !version[0] || !branch[0] || !broker_url[0] || !broker_username[0] || !broker_password[0] || !enable_pending_str[0] || !protocol[0] || !consumer_version_selectors_len_str[0] || !publish_verification_results_str[0]) {
        fprintf(stderr, "Missing required config variable(s): ");
        int first = 1;
        if (!name[0]) { fprintf(stderr, "%sname", first ? "" : ", "); first = 0; }
        if (!scheme[0]) { fprintf(stderr, "%sscheme", first ? "" : ", "); first = 0; }
        if (!host[0]) { fprintf(stderr, "%shost", first ? "" : ", "); first = 0; }
        if (!port_str[0]) { fprintf(stderr, "%sport", first ? "" : ", "); first = 0; }
        if (!path[0]) { fprintf(stderr, "%spath", first ? "" : ", "); first = 0; }
        if (!version[0]) { fprintf(stderr, "%sversion", first ? "" : ", "); first = 0; }
        if (!branch[0]) { fprintf(stderr, "%sbranch", first ? "" : ", "); first = 0; }
        if (!broker_url[0]) { fprintf(stderr, "%sbroker_url", first ? "" : ", "); first = 0; }
        if (!broker_username[0]) { fprintf(stderr, "%sbroker_username", first ? "" : ", "); first = 0; }
        if (!broker_password[0]) { fprintf(stderr, "%sbroker_password", first ? "" : ", "); first = 0; }
        if (!enable_pending_str[0]) { fprintf(stderr, "%senable_pending", first ? "" : ", "); first = 0; }
        if (!protocol[0]) { fprintf(stderr, "%sprotocol", first ? "" : ", "); first = 0; }
        if (!publish_verification_results_str[0]) { fprintf(stderr, "%spublish_verification_results", first ? "" : ", "); first = 0; }
        fprintf(stderr, "\n");
        return 1;
    }

    int port = atoi(port_str);
    int enable_pending = atoi(enable_pending_str);
    int consumer_version_selectors_len = atoi(consumer_version_selectors_len_str);
    int publish_verification_results = atoi(publish_verification_results_str);

    // Handle consumer_version_selectors as JSON array string from Erlang
    // Parse the JSON array into individual selector strings for the Pact FFI
    char **selector_strings = NULL;
    const char **selector_array = NULL;
    const char *const *selectors_ptr = NULL;
    int selectors_count = consumer_version_selectors_len;
    
    if (consumer_version_selectors[0] != '\0' && selectors_count > 0) {
        // Parse JSON array into individual strings
        int parsed_count = 0;
        selector_strings = parse_json_array_to_strings(consumer_version_selectors, &parsed_count);
        
        if (selector_strings && parsed_count > 0) {
            // Create const char** array for Pact FFI
            selector_array = malloc(sizeof(const char *) * (selectors_count + 1));
            if (selector_array) {
                for (int i = 0; i < selectors_count; i++) {
                    selector_array[i] = selector_strings[i];
                }
                selector_array[selectors_count] = NULL;
                selectors_ptr = (const char *const *)selector_array;
            }
        }
    }

    struct VerifierHandle *verifierhandle = pactffi_verifier_new_for_application(name, version);
    pactffi_verifier_set_no_pacts_is_error(verifierhandle, 0);
    pactffi_verifier_set_provider_info(verifierhandle, name, scheme, host, port, path);
    pactffi_verifier_add_provider_transport(verifierhandle, protocol, port, path, scheme);

    if (state_path[0] != '\0') {
        pactffi_verifier_set_provider_state(verifierhandle, state_path, 0, 1);
    }
    pactffi_verifier_set_verification_options(verifierhandle, 0, 5000);
    if (publish_verification_results == 1) {
        pactffi_verifier_set_publish_options(verifierhandle, version, NULL, NULL, -1, branch);
    }
    pactffi_verifier_broker_source_with_selectors(
        verifierhandle, broker_url, broker_username, broker_password, NULL, enable_pending, NULL, NULL, -1, branch,
        selectors_ptr, selectors_count, NULL, -1);
    int verification_output = pactffi_verifier_execute(verifierhandle);
    pactffi_verifier_shutdown(verifierhandle);

    // Cleanup allocated memory
    if (selector_array) {
        free(selector_array);
    }
    if (selector_strings) {
        free_string_array(selector_strings, selectors_count);
    }

    // Write verification_output to file if PACT_RESULT_FILE is set, then delete the file
    const char *result_file = getenv("PACT_RESULT_FILE");
    if (result_file && result_file[0] != '\0') {
        FILE *fp = fopen(result_file, "w");
        if (fp) {
            fprintf(fp, "%d\n", verification_output);
            fclose(fp);
        } else {
            fprintf(stderr, "Failed to open result file: %s\n", result_file);
        }
    }

    // Exit with 0 for success, 1 for any error (for parent process logic)
    return verification_output;
}
