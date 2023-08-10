-module(resource_interface).

-export([get_resource_with_child/3]).

get_resource_with_child(Port, Barcode, QueryParams) ->
    ResourceBaseUrl = "/resources-service/wms-resource/",
    PayLoad =
        [
            {<<"barcode">>, Barcode}
        ],
    Url = ResourceBaseUrl ++ "resources/getByBarcode" ++ get_query_string(QueryParams),
    BaseUrl = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/api-gateway",
    FinalUrl = BaseUrl ++ Url,
    make_post_request(FinalUrl, thoas:encode(PayLoad), inf, #{name => ?FUNCTION_NAME}).

make_post_request(Url, Payload, _Retries, _Map) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    TempResponse = hackney:request(post, Url, Headers, Payload, []),
    Response =
    case TempResponse of
        {ok, StatusCode, _headers, Client} when StatusCode == 200 orelse StatusCode == 201 ->
            {ok, RespBody} = hackney:body(Client),
            {ok, RespBody};
        {ok, StatusCode, _headers, Client} when StatusCode == 400 ->
            {ok, RespBody} = hackney:body(Client),
            {error, RespBody};
        _Else ->
            {error, failed}
    end,
    Response.

-spec get_query_string(list()) -> string().
get_query_string(_Options = []) ->
    "";
get_query_string(Options) ->
    lists:foldl(
        fun(Option, ParamAcc) ->
            case Option of
                lock_children -> add_param("lockChildren=true", ParamAcc);
                lock_parent -> add_param("lockParent=true", ParamAcc);
                unlock_children -> add_param("unlockChildren=true", ParamAcc);
                unlock_parent -> add_param("unlockParent=true", ParamAcc);
                update_children -> add_param("updateChildren=true", ParamAcc);
                {Key, Val} -> add_param(Key ++ "=" ++ Val, ParamAcc)
            end
        end,
        "",
        Options
    ).

add_param(ParamString, ParamAcc) ->
    case ParamAcc of
        "" -> ParamAcc ++ "?" ++ ParamString;
        _ -> ParamAcc ++ "&" ++ ParamString
    end.
