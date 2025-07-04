#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pact.h"

// Usage: All arguments are read from environment variables
int main() {
    // Read all required arguments from environment variables
    const char *name = getenv("PACT_NAME");
    const char *scheme = getenv("PACT_SCHEME");
    const char *host = getenv("PACT_HOST");
    const char *port_str = getenv("PACT_PORT");
    const char *path = getenv("PACT_PATH");
    const char *version = getenv("PACT_VERSION");
    const char *branch = getenv("PACT_BRANCH");
    const char *broker_url = getenv("PACT_BROKER_URL");
    const char *broker_username = getenv("PACT_BROKER_USERNAME");
    const char *broker_password = getenv("PACT_BROKER_PASSWORD");
    const char *enable_pending_str = getenv("PACT_ENABLE_PENDING");
    const char *protocol = getenv("PACT_PROTOCOL");
    const char *state_path = getenv("PACT_STATE_PATH");
    const char *consumer_version_selectors = getenv("CONSUMER_VERSION_SELECTORS");
    const char *consumer_version_selectors_len_str = getenv("CONSUMER_VERSION_SELECTORS_LEN");

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

    if (!name || !scheme || !host || !port_str || !path || !version || !branch || !broker_url || !broker_username || !broker_password || !enable_pending_str || !protocol || !state_path || !consumer_version_selectors || !consumer_version_selectors_len_str) {
        fprintf(stderr, "Missing required environment variable(s)\n");
        return 1;
    }

    int port = atoi(port_str);
    int enable_pending = atoi(enable_pending_str);
    int consumer_version_selectors_len = atoi(consumer_version_selectors_len_str);

    struct VerifierHandle *verifierhandle = pactffi_verifier_new_for_application(name, version);
    pactffi_verifier_set_no_pacts_is_error(verifierhandle, 0);
    pactffi_verifier_set_provider_info(verifierhandle, name, scheme, host, port, path);
    pactffi_verifier_add_provider_transport(verifierhandle, protocol, port, path, scheme);

    if (state_path[0] != '\0') {
        pactffi_verifier_set_provider_state(verifierhandle, state_path, 0, 1);
    }
    pactffi_verifier_set_verification_options(verifierhandle, 0, 5000);
    pactffi_verifier_set_publish_options(verifierhandle, version, NULL, NULL, -1, branch);
    pactffi_verifier_broker_source_with_selectors(
        verifierhandle, broker_url, broker_username, broker_password, NULL, enable_pending, NULL, NULL, -1, branch,
        (const char *const *)&consumer_version_selectors, consumer_version_selectors_len, NULL, -1);
    int verification_output = pactffi_verifier_execute(verifierhandle);
    pactffi_verifier_shutdown(verifierhandle);
    return verification_output;
}
