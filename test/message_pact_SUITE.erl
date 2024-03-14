-module(message_pact_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").


all() -> [{group, consumer}, {group, producer}].

groups() ->
    [
        {consumer, [animal_consume_message]},
        {producer, [verify_producer]}
    ].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(consumer, Config) ->
    PactRef = pact:v4(<<"animal_service">>, <<"weather_service">>),
    [{pact_ref, PactRef} | Config];
init_per_group(producer, Config) ->
    Config.

end_per_group(consumer, Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    ok;
end_per_group(producer, Config) ->
    Config.


animal_consume_message(Config) ->
    PactRef = ?config(pact_ref, Config),
    Message = pact:like(#{
        weather => #{
            temperature => 23.0,
            humidity => 75.5,
            wind_speed_kmh => 29
        },
        timestamp => <<"2024-03-14T10:22:13+05:30">>
    }),
    TestMessage = pact:interaction(PactRef,
    #{
        given => <<"weather data for animals">>,
        upon_receiving => <<"a weather data message">>,
        with_contents => Message
    }),
    ?assertMatch(ok, animal_service:process_weather_data(TestMessage)),
    % {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

verify_producer(_Config) ->
    %% TODO
    {ok, Port} = weather_service:start(0),
    Handle = pactffi_nif:new_verifier(<<"weather_service">>, <<"default">>),
    pactffi_nif:verifier_set_provider_info(Handle, <<"weather_service">>, <<"http">>, <<"127.0.0.1">>, Port, <<"/generate_weather">>),
    pactffi_nif:verifier_add_provider_transport(Handle, <<"message">>, Port, <<"/generate_weather">>, <<"http">>),
    pactffi_nif:verifier_set_publish_options(Handle, <<"default">>, <<"develop">>),
    pactffi_nif:verifier_add_file_source(Handle, <<"./pacts">>),
    ?assertEqual(0, pactffi_nif:verifier_execute(Handle)),
    pactffi_nif:verifier_shutdown(Handle),
    weather_service:stop().
