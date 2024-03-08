-module(pact_matchers).

-export([
    like/1,
    each_like/1,
    each_key/2,
    regex_match/2
]).

%% @doc Function for matching with type of the given term
-spec like(binary() | boolean() | number() | list() | map()) -> map().
like(Term) when (is_number(Term) orelse is_binary(Term) orelse is_boolean(Term)) ->
    #{
        <<"value">> => Term,
        <<"pact:matcher:type">> => <<"type">>
    };
like(Term) when (is_list(Term)) ->
    lists:foldr(
        fun(Elem, Acc) ->
            [?MODULE:like(Elem) | Acc]
        end,
        [],
        Term
    );
like(Term) when (is_map(Term)) ->
    Keys = maps:keys(Term),
    case lists:member(<<"pact:matcher:type">>, Keys) of
        true ->
            Term;
        false ->
            maps:map(
                fun(_Key, InitValue) ->
                    ?MODULE:like(InitValue)
                end,
                Term
            )
    end.

%% @doc Function for matching each entity inside a list with type of given term
-spec each_like(binary() | boolean() | number() | map() | list()) -> map().
each_like(Term) when (is_number(Term) orelse is_binary(Term) orelse is_boolean(Term)) ->
    #{
        <<"value">> => [Term],
        <<"pact:matcher:type">> => <<"type">>
    };
each_like(Term) when (is_list(Term)) ->
    #{
        <<"value">> => Term,
        <<"pact:matcher:type">> => <<"type">>
    };
each_like(Term) when (is_map(Term)) ->
    Keys = maps:keys(Term),
    Map =
        case lists:member(<<"pact:matcher:type">>, Keys) of
            true ->
                Term;
            false ->
                maps:map(
                    fun(_Key, InitValue) ->
                        ?MODULE:like(InitValue)
                    end,
                    Term
                )
        end,
    #{
        <<"value">> => [Map],
        <<"pact:matcher:type">> => <<"type">>
    }.

%% @doc Function for matching with regex
-spec regex_match(binary() | boolean() | number() | map() | list(), binary()) -> map().
regex_match(Value, Regex) ->
    #{
        <<"value">> => Value,
        <<"pact:matcher:type">> => <<"regex">>,
        <<"regex">> => Regex
    }.

%% @doc Function for matching each key inside a map with regex
-spec each_key(binary() | boolean() | number() | map() | list(), binary()) -> map().
each_key(Value, Regex) ->
    #{
        <<"value">> => Value,
        <<"pact:matcher:type">> => <<"eachKey">>,
        <<"rules">> => [
            ?MODULE:regex_match(<<"">>, Regex)
        ]
    }.
