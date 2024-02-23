-module(pact_matchers).

-export([
    integer_or_identifier/0,
    integer_or_identifier/1,
    float/0,
    float/1,
    string/0,
    string/1,
    bool/0,
    bool/1
]).

-export([
    like/1,
    each_like/1,
    each_key/2,
    regex_match/2
]).

%% @doc function for matching with type of the given term
-spec like(binary() | boolean() | number()) -> list().
like(Term) ->
    [
        {<<"value">>, Term},
        {<<"pact:matcher:type">>, <<"type">>}
    ].

%% @doc function for matching each entity inside a list with type of given term
-spec each_like(binary() | boolean() | number()) -> list().
each_like(Term) ->
    [
        {<<"value">>, [Term]},
        {<<"pact:matcher:type">>, <<"type">>}
    ].

%% @doc function for matching with regex
-spec regex_match(binary() | boolean() | number(), binary()) -> list().
regex_match(Value, Regex) ->
    [
        {<<"value">>, Value},
        {<<"pact:matcher:type">>, <<"regex">>},
        {<<"regex">>, Regex}
    ].

%% @doc function for matching each key inside a map with regex
-spec each_key(binary() | boolean() | number(), binary()) -> list().
each_key(Value, Regex) ->
    [
        {<<"value">>, Value},
        {<<"pact:matcher:type">>, <<"eachKey">>},
        {<<"rules">>, [
            ?MODULE:regex_match(<<"">>, Regex)
        ]}
    ].

%% @doc function for matching integer
-spec integer_or_identifier() -> list().
integer_or_identifier() ->
    ?MODULE:integer_or_identifier(1).
-spec integer_or_identifier(integer()) -> list().
integer_or_identifier(Value) ->
    like(Value).

%% @doc function for matching float
-spec float() -> list().
float() ->
    ?MODULE:float(1.0).
-spec float(float()) -> list().
float(Value) ->
    like(Value).

%% @doc function for matching string
-spec string() -> list().
string() ->
    ?MODULE:string(<<"">>).
-spec string(string()) -> list().
string(Value) ->
    like(Value).

%% @doc function for matching boolean
-spec bool() -> list().
bool() ->
    ?MODULE:bool(true).
-spec bool(boolean()) -> list().
bool(Value) ->
    like(Value).
