%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  API Handler for Test IMS Service
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(test_weather_api_handler).

-export([
    init/2,
    content_types_provided/2,
    handle_incoming_requests/2,
    content_types_accepted/2,
    allowed_methods/2,
    generate_message/3
]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {list(), cowboy_req:req(), any()}.
allowed_methods(Req1, State) ->
    Path = cowboy_req:path(Req1),
    Req =
        case cowboy_req:path_info(Req1) of
            undefined ->
                PathInfo1 = string:lexemes(Path, "/") -- [<<"test_weather">>],
                Req1#{path_info => PathInfo1};
            _ ->
                Req1
        end,
    %% When we use trail for a function, the pathinfo becomes
    %% undefined due to which we have handle it separately in a case clause
    PathInfo = cowboy_req:path_info(Req),
    case PathInfo of
        [] ->
            {[<<"GET">>], Req, State};
        [<<"generate_weather">>] ->
            {[<<"POST">>, <<"OPTIONS">>], Req, State};
        _Other ->
            {[<<"GET">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}
    end.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) ->
    {list(), cowboy_req:req(), any()}.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_incoming_requests}], Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) ->
    {list(), cowboy_req:req(), any()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get_request}], Req, State}.

-spec handle_incoming_requests(Req :: cowboy_req:req(), State :: any()) ->
    {stop, cowboy_req:req(), any()}.
handle_incoming_requests(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> -> handle_post_request(Req, State);
        <<"GET">> -> handle_get_request(Req, State)
    end.

handle_get_request(Req, State) ->
    PathInfo = cowboy_req:path_info(Req),
    case PathInfo of
        Other ->
            api_404(Req, State)
    end.

-spec handle_post_request(Req :: cowboy_req:req(), State :: any()) ->
    {stop, cowboy_req:req(), any()}.
handle_post_request(Req, State) ->
    PathInfo = cowboy_req:path_info(Req),
    case PathInfo of
        [<<"generate_weather">>] ->
            generate_weather_message(Req, State);
        _Other ->
            api_404(Req, State)
    end.

generate_weather_message(Req, State) ->
    {ok, Body, Request} = cowboy_req:read_body(Req),
    {ok, StateReq} = thoas:decode(Body),
    Description = maps:get(<<"description">>, StateReq, <<"">>),
    ArgsList = given_args_mapping(Description),
    Message = erlang:apply(test_weather_api_handler, generate_message, ArgsList),
    respond_with_status_code(200, thoas:encode(Message), Request, State).

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

given_args_mapping(Given) ->
    case Given of
        <<"">> -> [23.5, 20, 75.0];
        _ -> [24.5, 20, 93.0]
    end.

api_404(Req, State) ->
    respond_with_status_code(404, #{}, "Not Found", Req, State).

respond_with_status_code(StatusCode, Message, Req, State) ->
    respond_with_status_code(StatusCode, get_headers(), Message, Req, State).

-spec respond_with_status_code(
    cowboy:http_status(), cowboy:http_headers(), binary(), cowboy_req:req(), any()
) -> {stop, cowboy_req:req(), any()}.
respond_with_status_code(StatusCode, Headers, Message, Req, State) ->
    Req1 = cowboy_req:reply(StatusCode, Headers, Message, Req),
    {stop, Req1, State}.

-spec get_headers() -> map().
get_headers() ->
    #{<<"content-type">> => <<"application/json">>}.
