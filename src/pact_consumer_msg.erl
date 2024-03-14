-module(pact_consumer_msg).

-export([
    v4/2,
    interaction/2,
    cleanup_interaction/1
]).

-type pact_ref() :: integer().
-type pact_interaction_ref() :: integer().
-type consumer() :: binary().
-type provider() :: binary().
-type pact_pid() :: pid().
-type pact_interaction_details() :: map().
-type message() :: map().

-spec v4(consumer(), provider()) -> pact_pid().
v4(Consumer, Provider) ->
    {ok, PactPid} = pact_ref_server:start(Consumer, Provider),
    PactPid.

-spec interaction(pact_pid(), pact_interaction_details()) ->
    {ok, message()}.
interaction(PactPid, Interaction) ->
    {PactRef, InteractionRef} = init_interaction(PactPid, Interaction),
    Message = maps:get(with_contents, Interaction, #{}),
    ok = insert_contents(InteractionRef, Message).
    pactffi_nif:get_reified_message(InteractionRef).

-spec cleanup_interaction(pact_pid()) -> ok.
cleanup_interaction(PactPid) ->
    PactRef = pact_ref_server:get_pact_ref(PactPid),
    pactffi_nif:free_pact_handle(PactRef).


%% Internal functions

-spec init_interaction(pact_pid(), pact_interaction_details()) ->
    {pact_ref(), pact_interaction_ref()}.
init_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = pact_ref_server:get_consumer_producer(PactPid),
    PactRef = pactffi_nif:new_pact(Consumer, Producer),
    ok = pact_ref_server:set_pact_ref(PactPid, PactRef),
    InteractionDesc = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pactffi_nif:new_msg_interaction(PactRef, InteractionDesc),
    case maps:get(given, Interaction, undefined) of
        undefined ->
            ok;
        GivenState when is_binary(GivenState) ->
            pactffi_nif:msg_given(InteractionRef, GivenState);
        GivenStateWithParams when is_map(GivenStateWithParams) ->
            ProviderState = maps:get(state, GivenStateWithParams, <<"">>),
            StateJson = maps:get(params, GivenStateWithParams, undefined),
            case StateJson of
                undefined ->
                    pactffi_nif:msg_given(InteractionRef, ProviderState);
                _ ->
                    pactffi_nif:msg_given_with_params(InteractionRef, ProviderState, StateJson)
            end
    end,
    ok = pact_ref_server:create_interaction(PactPid, InteractionRef, Interaction),
    {PactRef, InteractionRef}.

-spec insert_contents(pact_interaction_ref(), message()) -> ok.
insert_contents(InteractionRef, Message) ->
    %% Checking if someone used regex_match
    CheckIfMap =
        fun(Value) ->
            case is_map(Value) of
                true ->
                    thoas:encode(Value);
                false ->
                    Value
            end
        end,
    ResHeaders = maps:get(headers, ResponseDetails, #{}),
    ContentType = get_content_type(ResHeaders),
    case Message of
        undefined ->
            ok;
        _ ->
            NewMessage = CheckIfMap(Message),
            pactffi_nif:msg_with_contents(InteractionRef, ContentType, NewMessage)
    end,
    ok.

-spec get_content_type(map()) -> binary().
get_content_type(Headers) ->
    maps:get(<<"Content-Type">>, Headers, <<"application/json">>).
