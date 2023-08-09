-module(resource_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [
    get_resource_with_child_test_success,
    get_resource_with_child_test_failure
].

%% ------------------------
%% Suite init and teardowns
%% ------------------------
init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    PactRef = pact:v4(<<"myapp">>, <<"resource-service">>),
    [{pact_ref, PactRef} | Config].

end_per_suite(Config) ->
    PactRef = ?config(pact_ref, Config),
    pact:cleanup(PactRef),
    application:stop(hackney),
    ok.

%% ----------------------
%% Test case 1
%% ----------------------
get_resource_with_child_test_success(Config) ->
    PactRef = ?config(pact_ref, Config),
    QueryParamValue = query_param_value(),
    RequestJson = request_body_success(),
    ResponseJson = response_body_success(),
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"/getByBarcode post api returns 200">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/api-gateway/resources-service/wms-resource/resources/getByBarcode">>,
            query_params => #{
                <<"includeChildren">> => QueryParamValue
            },
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => RequestJson
        },
        will_respond_with => #{
            status => 200,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => ResponseJson
        }
    }),
    QueryParams1 = [{"includeChildren", "false"}],
    ?assertMatch({ok, _}, resource_interface:get_resource_with_child(Port, <<"tote_2">>, QueryParams1)),
    {ok, matched} = pact:verify(PactRef),
    write_pact(PactRef).



%% ----------------------
%% Test case 2
%% ----------------------
get_resource_with_child_test_failure(Config) ->
    PactRef = ?config(pact_ref, Config),
    QueryParamValue = query_param_value(),
    RequestJson1 = request_body_failure(),
    ResponseJson1 = response_body_failure(),
    {ok, Port} = pact:interaction(PactRef,
    #{
        upon_receiving => <<"/getByBarcode post api returns 400">>,
        with_request => #{
            method => <<"POST">>,
            path => <<"/api-gateway/resources-service/wms-resource/resources/getByBarcode">>,
            query_params => #{
                <<"includeChildren">> => QueryParamValue
            },
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => RequestJson1
        },
        will_respond_with => #{
            status => 400,
            headers => #{
                <<"Content-Type">> => <<"application/json">>
            },
            body => ResponseJson1
        }
    }),
    QueryParams1 = [{"includeChildren", "true"}],
    ?assertMatch({error, _}, resource_interface:get_resource_with_child(Port, <<"tote_1">>, QueryParams1)),
    {ok, matched} = pact:verify(PactRef),
    write_pact(PactRef).



%% -------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------

query_param_value() ->
    jsx:encode([
        {<<"value">>, <<"false">>},
        {<<"pact:matcher:type">>, <<"regex">>},
        {<<"regex">>, <<"(true|false)">>}
    ]).

request_body_success() ->
    jsx:encode(
        [{<<"barcode">>, <<"tote_2">>}]
    ).

response_body_success() ->
    jsx:encode(
        [
            {<<"resource_id">>, [
                {<<"value">>, 10},
                {<<"pact:matcher:type">>, <<"type">>}
            ]},
            {<<"resource_barcode">>, [
                {<<"value">>, <<"tote_1">>},
                {<<"pact:matcher:type">>, <<"type">>}
            ]}
        ]
    ).

request_body_failure() ->
    jsx:encode(
        [{<<"barcode">>, [{<<"value">>, <<"tote_1">>}, {<<"pact:matcher:type">>, <<"type">>}]}]
    ).

response_body_failure() ->
    jsx:encode(
        [
            {<<"resource_id">>, <<"BAD_REQUEST">>},
            {<<"code">>, <<"ERR002">>},
            {<<"message">>, <<"Bad Request">>},
            {<<"reason">>, <<"Requested resource is not present">>}
        ]
    ).

-spec get_pacts_dir() -> binary().
get_pacts_dir() ->
    {ok, Cwd} = file:get_cwd(),
    CwdComponents = filename:split(Cwd),
    CurrProfileComponents = lists:sublist(CwdComponents, length(CwdComponents) - 2),
    PactDir = filename:join(CurrProfileComponents) ++ "/pacts",
    ok = filelib:ensure_dir(PactDir),
    PactDirBinary = list_to_binary(PactDir),
    PactDirBinary.

-spec write_pact(integer()) -> ok.
write_pact(PactRef) ->
    PactDirBinary = get_pacts_dir(),
    pact:write(PactRef, PactDirBinary).
