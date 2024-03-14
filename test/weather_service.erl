-module(weather_service).
-include_lib("inets/include/httpd.hrl").

-export([
    generate_message/2
]).

-export([
    start/0,
    start/1,
    stop/0,
    do/1
]).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, Pid} = inets:start(httpd, [{port, Port}, {server_name, "weather_service"}, {server_root, "./"}, {document_root, "./"}, {modules, [weather_service]}]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort}.

stop() ->
    inets:stop().

do(ModData) ->
    case catch process_data(ModData) of
        {'EXIT', Reason} ->
            io:format("Error: ~p~n", [Reason]),
            [{response, {500, "Internal Server Error"}}];
        Response ->
            Response
    end.

process_data(#mod{request_uri = "/generate_weather", method = "POST", entity_body = Body}) ->
    {ok, StateReq} = thoas:encode(Body),
    Description = maps:get(<<"description">>, StateReq, <<"">>),
    ArgsList = given_args_mapping(Description),
    Message = erlang:apply(weather_service, generate_message, ArgsList),
    make_json_response(200, Message).

process_data(#mod{request_uri = "/pactStateChange", method = "POST", entity_body = Body}) ->
    {ok, StateRequest} = thoas:decode(Body),
    RequiredState = maps:get(<<"state">>, StateRequest, <<"">>),
    case RequiredState of
        <<"">> -> reset_data();
        <<"an alligator with the name Mary exists">> ->
            case maps:get(<<"params">>, StateRequest, undefined) of
                undefined ->
                    insert_animal(<<"Mary">>, <<"alligator">>);
                Params ->
                    Name = maps:get(<<"name">>, Params, <<"">>),
                    Type = maps:get(<<"type">>, Params, <<"">>),
                    insert_animal(Name, Type)
            end;
        <<"a dog with the name Duke exists">> ->
            insert_animal(<<"Duke">>, <<"Dog">>)
    end,
    make_json_response(200, #{ok => true}).

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [{response, {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}], BodyJson}}]}.

make_404_response() ->
    make_json_response(404, #{error => not_found}).

generate_message(Temperature, WindSpeed, Humidity) ->
    #{
        weather => #{
            temperature => Temperature,
            humidity => Humidity,
            wind_speed_kmh => WindSpeed
        },
        timestamp => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second)))
    }.


given_args_mapping(Given) ->
    case Given of
        <<"">> -> [23.5, 20, 75.0];
        _ -> [24.5, 20, 93.0]
    end.
