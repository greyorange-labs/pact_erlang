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
            pactffi_nif:response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, #{}),
    ContentType = get_content_type(ResHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            %% FIXME: 4th parameter is Index.. need to increment
            pactffi_nif:with_header_v2(InteractionRef, 1, Key, 0, Value)
        end,
        ok,
        ResHeaders
    ),
    ResBody = maps:get(body, ResponseDetails, undefined),
    case ResBody of
        undefined -> ok;
        _ ->
            pactffi_nif:with_body(InteractionRef, 1, ContentType, ResBody)
    end,
    ok.


%% Internal functions

-spec get_content_type(map()) -> binary().
get_content_type(Headers) ->
    maps:get(<<"Content-Type">>, Headers, <<"application/json">>).
