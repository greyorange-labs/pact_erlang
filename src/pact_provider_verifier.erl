-module(pact_provider_verifier).
-include_lib("inets/include/httpd.hrl").


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
    % create_table(),
    {ok, Pid} = inets:start(httpd, [{bind_address, "127.0.0.1"}, {port, Port}, {server_name, "pact_provider_verifier"}, {server_root, "./"}, {document_root, "./"}, {modules, [pact_provider_verifier]}]),
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



process_data(#mod{request_uri = ReqUri, method = "GET"}) ->
    UriMap = uri_string:parse(ReqUri),
    Path = maps:get(path, UriMap),
    SplitPath = string:tokens(Path, "/"),
    case SplitPath of
        ["test"] ->
            % NameBinary = erlang:list_to_binary(Name),
            % case find_animal_by_name(NameBinary) of
            %     {ok, Animal} ->
            %         make_json_response(200, Animal);
            %     {error, not_found} ->
            %         make_404_response()
            % end;
            make_json_response(200, #{<<"hello">> => <<"moto">>});
        % ["animals"] ->
        %     QueryStr = maps:get(query, UriMap, ""),
        %     Query = maps:from_list(uri_string:dissect_query(QueryStr)),
        %     case maps:get("type", Query, undefined) of
        %         undefined ->
        %             make_json_response(400, #{error => <<"Missing query parameter: type">>});
        %         Type ->
        %             TypeBinary = erlang:list_to_binary(Type),
        %             AnimalObjectList = find_animals_by_type(TypeBinary),
        %             make_json_response(200, #{animals => AnimalObjectList})
        %     end;
        _ ->
            make_404_response()
    end;
process_data(#mod{request_uri = "/animals", method = "POST", entity_body = Body}) ->
    case thoas:decode(Body) of
        {ok, Animal} ->
            _Name = maps:get(<<"name">>, Animal, undefined),
            _Type = maps:get(<<"type">>, Animal, undefined),
            make_json_response(201, #{ok => true});
        {error, Reason} ->
            io:format("JSON Decode Error: ~p~n", [Reason]),
            make_json_response(400, #{error => <<"Invalid JSON">>})
    end;
process_data(#mod{request_uri = "/pactStateChange", method = "POST", entity_body = Body}) ->
    {ok, StateRequest} = thoas:decode(Body),
    RequiredState = maps:get(<<"state">>, StateRequest, <<"">>),
    case RequiredState of
        <<"">> -> ok;
        <<"an alligator with the name Mary exists">> ->
            case maps:get(<<"params">>, StateRequest, undefined) of
                undefined ->
                    ok;
                Params ->
                    _Name = maps:get(<<"name">>, Params, <<"">>),
                    _Type = maps:get(<<"type">>, Params, <<"">>),
                    ok
            end;
        <<"a dog with the name Duke exists">> ->
            ok
    end,
    make_json_response(200, #{ok => true}).

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [{response, {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}], BodyJson}}]}.

make_404_response() ->
    make_json_response(404, #{error => not_found}).
