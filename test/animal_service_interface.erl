-module(animal_service_interface).

-export([
    get_animal/2
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
