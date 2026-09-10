-module(pact_verifier).
-include_lib("inets/include/httpd.hrl").
-behaviour(gen_server).

%% Internal web server API
-export([
    start/2,
    stop/1,
    do/1
]).

%% Pact Verifier APIs
-export([
    start_verifier/2,
    verify/1
]).

-export([init/1, handle_call/3, terminate/2]).

-dialyzer(no_behaviours).

start(Port, Name) ->
    {ok, Pid} = inets:start(httpd, [
        {bind_address, "127.0.0.1"},
        {port, Port},
        {server_name, binary_to_list(Name)},
        {server_root, "./"},
        {document_root, "./"},
        {modules, [pact_verifier]}
    ]),
    Info = httpd:info(Pid),
    {port, ListenPort} = lists:keyfind(port, 1, Info),
    {ok, ListenPort, Pid}.

stop(Pid) ->
    inets:stop(httpd, Pid).

do(ModData) ->
    case catch process_data(ModData) of
        {'EXIT', Reason} ->
            io:format("Error: ~p~n", [Reason]),
            [{response, {500, "Internal Server Error"}}];
        Response ->
            Response
    end.

process_data(
    #mod{request_uri = "/message_pact/verify", method = "POST", entity_body = Body} = ModData
) ->
    {ok, StateReq} = thoas:decode(Body),
    Description = maps:get(<<"description">>, StateReq, <<"">>),
    Port = extract_address_port(ModData#mod.absolute_uri),
    Info = httpd:info({127, 0, 0, 1}, Port),
    {server_name, ServerName} = lists:keyfind(server_name, 1, Info),
    {M, F, Args} = get_mfa_from_description(ServerName, Description),
    Message = erlang:apply(M, F, Args),
    make_json_response(200, Message).

make_json_response(Code, Body) ->
    BodyJson = erlang:binary_to_list(thoas:encode(Body)),
    Length = io_lib:format("~w", [io_lib:chars_length(BodyJson)]),
    {proceed, [
        {response,
            {response, [{code, Code}, {content_length, Length}, {content_type, "application/json"}],
                BodyJson}}
    ]}.

%% Web Server End
%% ================================================================================================

%% Gen Server
-type provider() :: binary().
-type provider_opts() :: map().
-type verfier_ref() :: pid().

%% erlfmt-ignore
-record(pact_verifier, {
    provider                        :: provider(),
    provider_opts = #{}             :: provider_opts(),
    provider_port_details           :: tuple() 
}).

%% @doc Starts pact provider verifier
-spec start_verifier(provider(), provider_opts()) -> gen_server:start_ret().
start_verifier(Provider, ProviderOpts) ->
    Protocol = maps:get(protocol, ProviderOpts, <<"http">>),
    {Port, HttpPid} =
        case Protocol of
            <<"http">> ->
                Port1 = maps:get(port, ProviderOpts),
                {Port1, undefined};
            <<"message">> ->
                {ok, Port1, HttpPid1} = pact_verifier:start(0, Provider),
                {Port1, HttpPid1}
        end,
    gen_server:start(
        {global, {?MODULE, Provider}},
        ?MODULE,
        #pact_verifier{
            provider = Provider,
            provider_opts = ProviderOpts,
            provider_port_details = {Port, HttpPid}
        },
        []
    ).

-spec verify(verfier_ref()) -> integer().
verify(VerifierRef) ->
    {ProviderOpts, ProviderPortDetails} = gen_server:call(VerifierRef, {get_provider_state_details}),
    verify_pacts(VerifierRef, ProviderOpts, ProviderPortDetails).

-spec get_mfa_from_description(string(), binary()) -> tuple().
get_mfa_from_description(Provider, Description) ->
    {Map, FallbackProviderMFA} = gen_server:call({global, {?MODULE, list_to_binary(Provider)}}, {
        get_message_providers_map
    }),
    case maps:get(Description, Map, undefined) of
        undefined ->
            FallbackProviderMFA;
        ProviderMFA ->
            ProviderMFA
    end.

stop_verifier(VerfierRef) ->
    ok = gen_server:stop(VerfierRef).

%% message_providers map example
%% Sample description to MFA mapping for pact verifier to know which MFA to test
% #{
%     <<"weather api 1">> => {M, F, ArgsList}
% }

%% Gen_server callbacks

init(#pact_verifier{provider = _Provider, provider_opts = _ProviderOpts} = State) ->
    {ok, State}.

handle_call({get_provider_state_details}, _From, State) ->
    %% Verify file pacts or broker pacts or both here
    %% Based on pact_source_opts key in Opts.
    ProviderOpts = State#pact_verifier.provider_opts,
    ProviderPortDetails = State#pact_verifier.provider_port_details,
    {reply, {ProviderOpts, ProviderPortDetails}, State};
handle_call({get_message_providers_map}, _From, State) ->
    ProviderOpts = State#pact_verifier.provider_opts,
    MessageProvidersMap = maps:get(message_providers, ProviderOpts, #{}),
    FallbackProviderFunc = maps:get(fallback_message_provider, ProviderOpts, undefined),
    {reply, {MessageProvidersMap, FallbackProviderFunc}, State}.

terminate(_Reason, _State) ->
    ok.

verify_pacts(VerifierRef, ProviderOpts, ProviderPortDetails) ->
    {Port, HttpPid} = ProviderPortDetails,
    #{
        name := Name,
        version := Version,
        host := Host,
        branch := Branch,
        pact_source_opts := PactSourceOpts
    } = ProviderOpts,
    Protocol = maps:get(protocol, ProviderOpts, <<"http">>),
    BaseUrl = maps:get(base_url, ProviderOpts, <<"/">>),
    StateChangeUrl = maps:get(state_change_url, ProviderOpts, <<"">>),
    IncludeWipPactsSince = maps:get(include_wip_pacts_since, ProviderOpts, <<"">>),
    SkipPublish = maps:get(skip_publish, ProviderOpts, <<"0">>),
    Scheme = maps:get(scheme, ProviderOpts, <<"http">>),
    BuildUrl = maps:get(build_url, ProviderOpts, <<"">>),
    FilePath = maps:get(file_path, PactSourceOpts, undefined),
    PactUrl = maps:get(pact_url, PactSourceOpts, undefined),
    PactBrokerUrl = maps:get(broker_url, PactSourceOpts, undefined),
    BrokerUser = maps:get(broker_username, PactSourceOpts, <<"username">>),
    BrokerPassword = maps:get(broker_password, PactSourceOpts, <<"password">>),
    EscriptPath = code:priv_dir(pact_erlang) ++ "/pact_escript.escript",
    
    FilePathOutput =
        case FilePath of
            undefined ->
                0;
            _ ->
                FilePathArgs =
                    [
                        Name,
                        Scheme,
                        Host,
                        Port,
                        BaseUrl,
                        Version,
                        Branch,
                        FilePath,
                        Protocol,
                        StateChangeUrl
                    ],
                {FilePathExecOutput, FilePathExecOutputLog} = pact_utils:run_executable_async(
                    EscriptPath,
                    ["pactffi_nif", "verify_file_pacts" | args_to_strings(FilePathArgs)]
                ),
                io:format(FilePathExecOutputLog),
                FilePathExecOutput
        end,
    PactUrlOutput =
        case PactUrl of
            undefined ->
                0;
            _ ->
                PactUrlArgs =
                    [
                        Name,
                        Scheme,
                        Host,
                        Port,
                        BaseUrl,
                        Version,
                        Branch,
                        PactUrl,
                        Protocol,
                        StateChangeUrl,
                        BrokerUser,
                        BrokerPassword,
                        BuildUrl
                    ],
                {PactUrlExecOutput, PactUrlExecOutputLog} = pact_utils:run_executable_async(
                    EscriptPath,
                    ["pactffi_nif", "verify_url_pacts" | args_to_strings(PactUrlArgs)]
                ),
                io:format(PactUrlExecOutputLog),
                PactUrlExecOutput
        end,
    PactBrokerOutput =
        case PactBrokerUrl of
            undefined ->
                0;
            _ ->
                EnablePending = maps:get(enable_pending, PactSourceOpts, <<"0">>),
                ConsumerVersionSelectors = maps:get(consumer_version_selectors, PactSourceOpts, undefined),
                PactBrokerArgs = [
                    Name,
                    Scheme,
                    Host,
                    Port,
                    BaseUrl,
                    Version,
                    Branch,
                    PactBrokerUrl,
                    BrokerUser,
                    BrokerPassword,
                    EnablePending,
                    ConsumerVersionSelectors,
                    Protocol,
                    StateChangeUrl,
                    SkipPublish,
                    IncludeWipPactsSince
                ],
                {PactBrokerExecOutput, PactBrokerExecOutputLog} = pact_utils:run_executable_async(
                    EscriptPath,
                    ["pactffi_nif", "verify_broker_pacts" | args_to_strings(PactBrokerArgs)]
                ),
                io:format(PactBrokerExecOutputLog),
                PactBrokerExecOutput
        end,
    case Protocol of
        <<"message">> ->
            pact_verifier:stop(HttpPid),
            stop_verifier(VerifierRef);
        _ ->
            stop_verifier(VerifierRef)
    end,
    combine_return_codes(FilePathOutput, PactUrlOutput, PactBrokerOutput).

%% Escript arguments are passed as separate, individually quoted strings so
%% that empty arguments keep their position instead of being swallowed by the
%% shell, which would shift every following argument by one.
args_to_strings(Args) ->
    [
        case Arg of
            Int when is_integer(Int) -> integer_to_list(Int);
            Bin -> binary_to_list(Bin)
        end
     || Arg <- Args
    ].

combine_return_codes(0, 0, 0) -> 0;
combine_return_codes(Code1, _, _) when Code1 =/= 0 -> Code1;
combine_return_codes(_, Code2, _) when Code2 =/= 0 -> Code2;
combine_return_codes(_, _, Code3) -> Code3.

extract_address_port(Url) ->
    % Define a regex pattern to match the address and port
    Pattern = ":(\\d+)/",

    % Use re module to match the pattern against the URL
    case re:run(Url, Pattern, [{capture, all_but_first, list}]) of
        {match, [PortStr]} ->
            % Convert the port string to an integer
            Port = list_to_integer(PortStr),
            Port
    end.
