-module(pact_verifier_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/httpd.hrl").

all() -> [test_api_handler_failure, pact_verifier_handle_cast_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_api_handler_failure(_Config) ->
    ModData = #mod{request_uri = "/message_pact/verify", method = "GET"},
    ?assertEqual([{response, {500, "Internal Server Error"}}], pact_verifier:do(ModData)).


pact_verifier_handle_cast_test(_Config) ->
    Provider = <<"test_provider">>,
    ProviderOpts = #{protocol => <<"http">>, port => 1234},
    {ok, Pid} = pact_verifier:start_verifier(Provider, ProviderOpts),
    ok = gen_server:cast(Pid, test_cast_message),
    pact_verifier:stop_verifier(Pid),
    ok.
