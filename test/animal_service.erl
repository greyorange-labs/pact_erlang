-module(animal_service).
-include_lib("inets/include/httpd.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    do/1
]).

-define(TABLE_NAME, animals).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(inets),
    create_table(),
    insert_animal("Mary", "alligator"),
    {ok, Pid} = inets:start(httpd, [{port, Port}, {server_name, "animal_service"}, {server_root, "./"}, {document_root, "./"}, {modules, [animal_service]}]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort}.

stop() ->
    catch ets:delete(?TABLE_NAME),
    inets:stop().

create_table() ->
    ets:new(?TABLE_NAME, [set, public, named_table]).

insert_animal(Name, Type) ->
    ets:insert(?TABLE_NAME, {Name, Type}).

find_animal_by_name(Name) ->
    case ets:match_object(?TABLE_NAME, {Name, '_'}) of
        [] ->
            {error, not_found};
        [{Name, Type}] ->
            {ok, #{name => erlang:list_to_binary(Name), type => erlang:list_to_binary(Type)}}
    end.

do(ModData) ->
    case catch process_data(ModData) of
        {'EXIT', Reason} ->
            io:format("Error: ~p~n", [Reason]),
            [{response, {500, "Internal Server Error"}}];
        Response ->
            Response
    end.

process_data(#mod{request_uri = Path}) ->
    SplitPath = string:tokens(Path, "/"),
    ResponseData = case SplitPath of
        ["animals", Name] ->
            case find_animal_by_name(Name) of
                {ok, Animal} ->
                    make_json_response(200, Animal);
                {error, not_found} ->
                    make_404_response()
            end;
        _ ->
            make_404_response()
    end,
    {proceed, ResponseData}.

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    [{response, {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}], BodyJson}}].

make_404_response() ->
    make_json_response(404, #{error => not_found}).
