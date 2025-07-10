-module(weather_service).
-include_lib("inets/include/httpd.hrl").

-export([
    start/0,
    start/1,
    stop/1,
    do/1,
    process_weather_data/1
]).


-export([
    generate_message/3
]).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, Pid} = inets:start(httpd, [{bind_address, "127.0.0.1"}, {port, Port}, {server_name, "weather_service"}, {server_root, "./"}, {document_root, "./"}, {modules, [weather_service]}]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort, Pid}.

stop(Pid) ->
    inets:stop(httpd, Pid).


do(ModData) ->
    case catch process_data(ModData) of
        {'EXIT', Reason} ->
            io:format("Error: ~p~n", [Reason]),
            [{response, {500, "Internal Server Error"}}];
        Response ->
            Response
    end.

process_data(#mod{request_uri = "/", method = "POST", entity_body = Body}) ->
    {ok, StateRequest} = thoas:decode(Body),
    io:format(user, "State Request: ~p~n", [StateRequest]),
    RequiredState = maps:get(<<"state">>, StateRequest, <<"">>),
    Message =
        case RequiredState of
            <<"">> -> generate_message(23.5, 20, 75.0);
            _ -> generate_message(24.5, 20, 93.0)
        end,
    make_json_response(200, Message);
process_data(_ModData) ->
    make_404_response().

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [{response, {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}], BodyJson}}]}.

make_404_response() ->
    make_json_response(404, #{error => not_found}).

process_weather_data(Payload) ->
    #{
        <<"weather">> := #{
            <<"temperature">> := _Temp,
            <<"humidity">> := _Humidity,
            <<"wind_speed_kmh">> := _WindSpeed
        },
        <<"timestamp">> := _TimeStamp
    } = Payload,
    %% Do something with weather data like validation,
    %% Db update
    ok.

generate_message(Temperature, WindSpeed, Humidity) ->
    #{
        <<"weather">> => #{
            <<"temperature">> => Temperature,
            <<"humidity">> => Humidity,
            <<"wind_speed_kmh">> => WindSpeed
        },
        <<"timestamp">> => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(second))
        )
    }.
