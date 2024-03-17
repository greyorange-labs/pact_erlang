-module(message_pact_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%% {group, consumer}, {group, producer}

all() -> [{group, consumer}, {group, producer}].

groups() ->
    [
        {consumer, [animal_consume_message]},
        {producer, [verify_producer]}
    ].


init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
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
    TestMessage = pact:msg_interaction(PactRef,
    #{
        given => <<"weather data for animals">>,
        upon_receiving => <<"a weather data message">>,
        with_contents => Message
    }),
    #{<<"contents">> := TestMessageContents} = TestMessage,
    ?assertMatch(ok, animal_service:process_weather_data(TestMessageContents)),
    % {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

verify_producer(_Config) ->
    %% TODO
    Port = init_test_api_handler(),
    Name = <<"weather_service">>,
    Version =  <<"default">>,
    Scheme = <<"http">>,
    Host = <<"localhost">>,
    Path = <<"/test_weather/generate_weather">>,
    Branch = <<"develop">>,
    FilePath = <<"./pacts">>,
    Protocol = <<"message">>,
    ?assertEqual(0, pactffi_nif:verify_via_file(Name, Scheme, Host, Port, Path, Version, Branch, FilePath, Protocol)),
    stop_test_api_handler(),
    ok.

init_test_api_handler() ->
    Dispatch = cowboy_router:compile([
        {<<"localhost">>, [
            {"/test_weather/[...]", test_weather_api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        test_weather_api_handler,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    Dispatch,
    Port = ranch:get_port(test_weather_api_handler),
    Port.

stop_test_api_handler() ->
    ok = cowboy:stop_listener(test_weather_api_handler).
