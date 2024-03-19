-module(message_pact_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%% {group, consumer}, {group, producer}

all() -> [{group, consumer}, {group, producer}, {group, verification}].

groups() ->
    [
        {consumer, [animal_consume_message]},
        {producer, [verify_producer]},
        {verification, [verification]}
    ].


init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    my_server:start_link(),
    my_server:start_server(),
    pactffi_nif:logger_init(),
    pactffi_nif:logger_attach_sink(<<"stdout">>, 4),
    pactffi_nif:logger_apply(),
    Config.

end_per_suite(_Config) ->
    my_server:stop_server(),
    application:stop(cowboy),
    ok.

init_per_group(consumer, Config) ->
    PactRef = pact:v4(<<"animal_service">>, <<"weather_service">>),
    [{pact_ref, PactRef} | Config];
init_per_group(producer, Config) ->
    % {ok, _} = my_server:start_link(),
    % my_server:start_server(),
    Config;
init_per_group(_, Config) ->
    % {ok, _} = my_server:start_link(),
    % my_server:start_server(),
    Config.

end_per_group(consumer, Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    ok;
end_per_group(producer, _Config) ->
    ok;
end_per_group(_, _Config) ->
    ok.


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
    pact:write(PactRef).

verify_producer(_Config) ->
    ok.


verification(_Config) ->
    Port = 8080,
    Name = <<"weather_service">>,
    Version =  <<"default">>,
    Scheme = <<"http">>,
    Host = <<"localhost">>,
    Path = <<"/test_weather/generate_weather">>,
    Branch = <<"develop">>,
    FilePath = <<"./pacts">>,
    Protocol = <<"message">>,
    % ok = create_animal(8080),
    Output = pactffi_nif:verify_via_file(Name, Scheme, Host, Port, Path, Version, Branch, FilePath, Protocol),
    ?assertEqual(0, Output).


create_animal(Port) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/test_weather/generate_weather",
    case httpc:request(post, {Url, [], "application/json", thoas:encode(#{<<"description">> => <<"given">>})}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ct:pal("executed generate weather api"),
            ok;
            %% {ok, Decoded} = thoas:decode(Body),
            %% {ok, Decoded};
        {error, Reason} ->
            ct:pal("failed generate weather api ~p",[Reason]),
            {error, Reason}
    end.

random_animal(Port) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/test_weather/some",
    case httpc:request(post, {Url, [], "application/json", thoas:encode(#{<<"description">> => <<"given">>})}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ct:pal("executed generate weather api"),
            ok;
            %% {ok, Decoded} = thoas:decode(Body),
            %% {ok, Decoded};
        {error, Reason} ->
            ct:pal("failed generate weather api ~p",[Reason]),
            {error, Reason}
    end.