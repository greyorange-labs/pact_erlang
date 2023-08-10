-module(request_builder).


-export([
    insert_request_details/2
]).


-type request_details() :: map().
-type pact_interaction_ref() :: integer().


-spec insert_request_details(pact_interaction_ref(), request_details()) -> ok.
insert_request_details(InteractionRef, RequestDetails) ->
    ReqMethod = maps:get(method, RequestDetails),
    ReqPath = maps:get(path, RequestDetails),
    pact_nif_interface:with_request(InteractionRef, ReqMethod, ReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, #{}),
    ContentType = get_content_type(ReqHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            pact_nif_interface:with_request_header(InteractionRef, Key, 0, Value)
        end,
        ok,
        ReqHeaders
    ),
    ReqBody = maps:get(body, RequestDetails, undefined),
    case ReqBody of
        undefined -> ok;
        _ ->
            pact_nif_interface:with_request_body(InteractionRef, ContentType, ReqBody)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_nif_interface:with_query_parameter(InteractionRef, Key, 0, Value)
                end,
                ok,
                ReqQueryParams
            )
    end,
    ok.


%% Internal functions

-spec get_content_type(map()) -> binary().
get_content_type(Headers) ->
    maps:get(<<"Content-Type">>, Headers, <<"application/json">>).
