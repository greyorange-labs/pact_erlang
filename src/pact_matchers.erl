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

-spec like(binary() | boolean() | number()) -> thoas:json_term().
like(Term) ->
    [
        {<<"value">>, Term},
        {<<"pact:matcher:type">>, <<"type">>}
    ].

-spec each_like(binary() | boolean() | number()) -> thoas:json_term().
each_like(Term) ->
    [
        {<<"value">>, [Term]},
        {<<"pact:matcher:type">>, <<"type">>}
    ].

-spec regex_match(binary() | boolean() | number(), binary()) -> thoas:json_term().
regex_match(Value, Regex) ->
    [
        {<<"value">>, Value},
        {<<"pact:matcher:type">>, <<"regex">>},
        {<<"regex">>, Regex}
    ].

-spec each_key(binary() | boolean() | number(), binary()) -> thoas:json_term().
each_key(Value, Regex) ->
    [
        {<<"value">>, Value},
        {<<"pact:matcher:type">>, <<"eachKey">>},
        {<<"rules">>, [
            ?MODULE:regex_match(<<"">>, Regex)
        ]}
    ].

integer_or_identifier() ->
    ?MODULE:integer_or_identifier(1).
integer_or_identifier(Value) ->
    like(Value).

float() ->
    ?MODULE:float(1.0).
float(Value) ->
    like(Value).

string() ->
    ?MODULE:string(<<"">>).
string(Value) ->
    like(Value).

bool() ->
    ?MODULE:bool(true).
bool(Value) ->
    like(Value).
