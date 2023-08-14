-module(pact_end_to_end_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [{group, consumer}, {group, producer}].

groups() ->
    [
        {consumer, [get_animal_success, get_animal_failure]},
        {producer, [verify_producer]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(consumer, Config) ->
    PactRef = pact:v4(<<"myapp">>, <<"animal_service">>),
    [{pact_ref, PactRef} | Config];
init_per_group(producer, Config) ->
    Config.

end_per_group(consumer, Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    ok;
end_per_group(producer, Config) ->
    Config.

get_animal_success(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalObject = #{<<"name">> => <<"Mary">>, <<"type">> => <<"alligator">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to GET an existing animal: Mary">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals/Mary">>
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(AnimalObject)
        }
    }),
    ?assertMatch({ok, AnimalObject}, animal_service_interface:get_animal(Port, "Mary")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef, <<"./pacts">>).

get_animal_failure(Config) ->
    PactRef = ?config(pact_ref, Config),
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to GET a non-existing animal: Max">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals/Max">>
        },
        will_respond_with => #{
            status => 404,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(#{error => not_found})
        }
    }),
    ?assertMatch({error, not_found}, animal_service_interface:get_animal(Port, "Max")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef, <<"./pacts">>).

verify_producer(_Config) ->
    {ok, Port} = animal_service:start(0),
    Cmd = "docker run --network host --rm -v ./pacts:/pacts "
            "pactfoundation/pact-ref-verifier --full-log "
            "-d /pacts -n animal_service -p " ++ integer_to_list(Port),
    {RetCode, Output} = run_cmd(Cmd),
    ct:print("===> Verification Output: ~n~s", [Output]),
    ?assertEqual(0, RetCode),
    animal_service:stop().

run_cmd(Cmd) ->
    Res = os:cmd(Cmd ++ "\nRET_CODE=$?\necho \"\n$RET_CODE\""),
    [[], RetCode | Rest] = lists:reverse(string:split(Res, "\n", all)),
    Result = lists:join("\n", lists:reverse(Rest)),
    {list_to_integer(RetCode), Result}.