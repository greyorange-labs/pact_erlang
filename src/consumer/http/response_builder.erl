-module(response_builder).


-export([
    insert_response_details/2
]).


-type response_details() :: map().
-type pact_interaction_ref() :: integer().


-spec insert_response_details(pact_interaction_ref(), response_details()) -> ok.
insert_response_details(InteractionRef, ResponseDetails) ->
    ResponseStatusCode = maps:get(status, ResponseDetails, undefined),
    case ResponseStatusCode of
        undefined -> ok;
        _ ->
            pact_nif_interface:with_response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, #{}),
    ContentType = get_content_type(ResHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            pact_nif_interface:with_response_header(InteractionRef, Key, 0, Value)
        end,
        ok,
        ResHeaders
    ),
    ResBody = maps:get(body, ResponseDetails, undefined),
    case ResBody of
        undefined -> ok;
        _ ->
            pact_nif_interface:with_response_body(InteractionRef, ContentType, ResBody)
    end,
    ok.


%% Internal functions

-spec get_content_type(map()) -> binary().
get_content_type(Headers) ->
    maps:get(<<"Content-Type">>, Headers, <<"application/json">>).
