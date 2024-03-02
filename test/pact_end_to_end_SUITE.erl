-module(pact_end_to_end_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [{group, consumer}, {group, producer}].

groups() ->
    [
        {consumer, [get_animal_success, get_animal_failure, create_animal, search_animals]},
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
        given => #{
            description => <<"an alligator with the name Mary exists">>,
            parameters => thoas:encode(AnimalObject)
        },
        upon_receiving => <<"a request to GET an animal: Mary">>,
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
    pact:write(PactRef).

get_animal_failure(Config) ->
    PactRef = ?config(pact_ref, Config),
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to GET a non-existing animal: Miles">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals/Miles">>
        },
        will_respond_with => #{
            status => 404,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(#{error => not_found})
        }
    }),
    ?assertMatch({error, not_found}, animal_service_interface:get_animal(Port, "Miles")),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

create_animal(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalObject = #{<<"name">> => <<"Max">>, <<"type">> => <<"dog">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"a request to create an animal: Max">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/animals">>,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(AnimalObject)
        },
        will_respond_with => #{
            status => 201
        }
    }),
    ?assertMatch(ok, animal_service_interface:create_animal(Port, AnimalObject)),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

search_animals(Config) ->
    PactRef = ?config(pact_ref, Config),
    AnimalObj = #{<<"name">> => <<"Mary">>, <<"type">> => <<"alligator">>},
    Result = #{<<"animals">> => [#{<<"name">> => <<"Mary">>, <<"type">> => <<"alligator">>}]},
    Query = #{<<"type">> => <<"alligator">>},
    {ok, Port} = pact:interaction(PactRef,
    #{
        given => #{
            description => <<"an alligator with the name Mary exists">>,
            parameters => thoas:encode(AnimalObj)
        },
        upon_receiving => <<"a request to find all alligators">>,
        with_request => #{
            method => <<"GET">>,
            path => <<"/animals">>,
            query_params => Query
        },
        will_respond_with => #{
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => thoas:encode(Result)
        }
    }),
    ?assertMatch({ok, Result}, animal_service_interface:search_animals(Port, Query)),
    {ok, matched} = pact:verify(PactRef),
    pact:write(PactRef).

verify_producer(_Config) ->
    {ok, Port} = animal_service:start(0),
    Cmd = "docker run --network host --rm -v ./pacts:/pacts "
            "-e PACT_DO_NOT_TRACK=true "
            "pactfoundation/pact-ref-verifier --full-log -l warn "
            "-s http://localhost:" ++ integer_to_list(Port) ++ "/pactStateChange "
            "-d /pacts -n animal_service -p " ++ integer_to_list(Port),
    {RetCode, Output} = run_cmd(Cmd),
    ct:print("===> Provider Verification Output: ~n~s", [Output]),
    ?assertEqual(0, RetCode),
    animal_service:stop().

run_cmd(Cmd) ->
    Res = os:cmd(Cmd ++ "\nRET_CODE=$?\necho \"\n$RET_CODE\""),
    [[], RetCode | Rest] = lists:reverse(string:split(Res, "\n", all)),
    Result = lists:join("\n", lists:reverse(Rest)),
    {list_to_integer(RetCode), Result}.
