-module(pactffi_plugin_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PACT_SPEC_V4, 5).
-define(INTERACTION_PART_REQUEST, 0).
-define(MISSING_PLUGIN, <<"pact-erlang-plugin-that-does-not-exist">>).

all() ->
    [
        with_specification_writes_v4_pact,
        new_sync_message_interaction_writes_sync_message,
        using_plugin_not_found,
        cleanup_plugins_without_plugins,
        interaction_contents_with_core_content_type,
        interaction_contents_with_invalid_json,
        write_pact_file_without_mock_server,
        cleanup_twice_after_write
    ].

with_specification_writes_v4_pact(Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"spec_provider">>),
    ?assertEqual({ok, specification_set}, pactffi_nif:with_specification(PactRef, ?PACT_SPEC_V4)),
    _ = pactffi_nif:new_interaction(PactRef, <<"a request">>),
    Pact = write_and_read_pact(Config, PactRef, <<"plugin_consumer-spec_provider.json">>),
    ?assertMatch(#{<<"metadata">> := #{<<"pactSpecification">> := #{<<"version">> := <<"4.0">>}}}, Pact).

new_sync_message_interaction_writes_sync_message(Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"sync_provider">>),
    {ok, specification_set} = pactffi_nif:with_specification(PactRef, ?PACT_SPEC_V4),
    InteractionRef = pactffi_nif:new_sync_message_interaction(PactRef, <<"a sync message">>),
    %% Handles are uint32 values returned as signed ints, so they can be negative
    ?assert(is_integer(InteractionRef)),
    #{<<"interactions">> := [Interaction]} =
        write_and_read_pact(Config, PactRef, <<"plugin_consumer-sync_provider.json">>),
    ?assertMatch(
        #{<<"type">> := <<"Synchronous/Messages">>, <<"description">> := <<"a sync message">>},
        Interaction
    ).

%% Fetches the plugin index over the network before failing
using_plugin_not_found(_Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"missing_plugin_provider">>),
    {error, 2, Message} = pactffi_nif:using_plugin(PactRef, ?MISSING_PLUGIN, <<"0.0.1">>),
    ?assert(byte_size(Message) > 0),
    ok = pactffi_nif:cleanup_plugins(PactRef),
    ok = pactffi_nif:free_pact_handle(PactRef).

cleanup_plugins_without_plugins(_Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"no_plugin_provider">>),
    ?assertEqual(ok, pactffi_nif:cleanup_plugins(PactRef)),
    ok = pactffi_nif:free_pact_handle(PactRef).

interaction_contents_with_core_content_type(_Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"contents_provider">>),
    InteractionRef = pactffi_nif:new_interaction(PactRef, <<"a request with contents">>),
    ?assertEqual(
        ok,
        pactffi_nif:interaction_contents(
            InteractionRef, ?INTERACTION_PART_REQUEST, <<"application/json">>, <<"{\"id\": 1}">>
        )
    ),
    ok = pactffi_nif:free_pact_handle(PactRef).

interaction_contents_with_invalid_json(_Config) ->
    PactRef = pactffi_nif:new_pact(<<"plugin_consumer">>, <<"invalid_contents_provider">>),
    InteractionRef = pactffi_nif:new_interaction(PactRef, <<"a request with invalid contents">>),
    {error, 5, Message} = pactffi_nif:interaction_contents(
        InteractionRef, ?INTERACTION_PART_REQUEST, <<"application/json">>, <<"{not json">>
    ),
    ?assert(byte_size(Message) > 0),
    ok = pactffi_nif:free_pact_handle(PactRef).

write_pact_file_without_mock_server(Config) ->
    Dir = list_to_binary(?config(priv_dir, Config)),
    ?assertEqual({error, 3}, pactffi_nif:write_pact_file(1, Dir, 0)).

cleanup_twice_after_write(Config) ->
    PactPid = pact:v4(<<"plugin_consumer">>, <<"cleanup_provider">>),
    {ok, _Port} = pact:interaction(PactPid, #{
        upon_receiving => <<"a request to cleanup">>,
        with_request => #{method => <<"GET">>, path => <<"/cleanup">>},
        will_respond_with => #{status => 200}
    }),
    Dir = list_to_binary(?config(priv_dir, Config)),
    ok = pact:write(PactPid, Dir),
    ?assertEqual(undefined, pact_ref_server:get_pact_ref(PactPid)),
    ?assertEqual(undefined, pact_ref_server:get_mock_server_port(PactPid)),
    %% Calls after the pact was written keep returning what they returned before
    ?assertEqual({error, not_matched}, pact:verify(PactPid)),
    ?assertEqual(ok, pact:write(PactPid, Dir)),
    ?assertEqual(ok, pact:cleanup(PactPid)).

%% Internal functions

write_and_read_pact(Config, PactRef, FileName) ->
    Dir = ?config(priv_dir, Config),
    0 = pactffi_nif:pact_handle_write_file(PactRef, list_to_binary(Dir), 1),
    ok = pactffi_nif:free_pact_handle(PactRef),
    {ok, Json} = file:read_file(filename:join(Dir, FileName)),
    {ok, Pact} = thoas:decode(Json),
    Pact.
