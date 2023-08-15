-module(animal_service_interface).

-export([
    get_animal/2,
    create_animal/2,
    search_animals/2
]).

get_animal(Port, Name) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/animals/" ++ Name,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Decoded} = thoas:decode(Body),
            {ok, Decoded};
        {ok, {{_, 404, _}, _, _Body}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

create_animal(Port, AnimalObject) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/animals",
    case httpc:request(post, {Url, [], "application/json", thoas:encode(AnimalObject)}, [], []) of
        {ok, {{_, 201, _}, _, _}} ->
            ok;
            %% {ok, Decoded} = thoas:decode(Body),
            %% {ok, Decoded};
        {error, Reason} ->
            {error, Reason}
    end.

search_animals(Port, Query) ->
    BaseUrl = "http://localhost:" ++ integer_to_list(Port) ++ "/animals",
    Url = BaseUrl ++ "?" ++ uri_string:compose_query(maps:to_list(Query)),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Decoded} = thoas:decode(Body),
            {ok, Decoded};
        {error, Reason} ->
            {error, Reason}
    end.
