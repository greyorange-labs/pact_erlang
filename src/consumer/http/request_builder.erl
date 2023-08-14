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
    pactffi_nif:with_request(InteractionRef, ReqMethod, ReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, #{}),
    ContentType = get_content_type(ReqHeaders),
    maps:fold(
        fun(Key, Value, _Acc) ->
            %% FIXME: 4th parameter is Index.. need to increment
            pactffi_nif:with_header_v2(InteractionRef, 0, Key, 0, Value)
        end,
        ok,
        ReqHeaders
    ),
    ReqBody = maps:get(body, RequestDetails, undefined),
    case ReqBody of
        undefined -> ok;
        _ ->
            pactffi_nif:with_body(InteractionRef, 0, ContentType, ReqBody)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    %% FIXME: 3rd parameter is Index.. need to increment
                    pactffi_nif:with_query_parameter_v2(InteractionRef, Key, 0, Value)
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
