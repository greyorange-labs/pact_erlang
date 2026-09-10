#!/usr/bin/env escript


main([Module, Function | Args]) ->
    add_pact_erlang_to_code_path(),
    ModuleAtom = list_to_atom(Module),
    FunctionAtom = list_to_atom(Function),
    ArgsList =
    case FunctionAtom of
        verify_file_pacts ->
            {AList, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        3 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList) < 10 of
                true ->
                    AList ++ [<<"">>];
                false ->
                    AList
            end;
        verify_url_pacts ->
            {AList, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        3 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList) < 13 of
                true ->
                    AList ++ [<<"">>];
                false ->
                    AList
            end;
        verify_broker_pacts ->
            {AList1, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        Num when Num == 3 orelse Num == 10 orelse Num == 14 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList1) < 16 of
                true ->
                    AList1 ++ [<<"">>];
                false ->
                    AList1
            end
    end,
    pact:enable_logging(info),
    Result = erlang:apply(ModuleAtom, FunctionAtom, ArgsList),
    case Result of
        ReturnValue when is_integer(ReturnValue) ->
            halt(ReturnValue);
        {badrpc, Reason} ->
            io:format("Error executing function: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: ./script.erl Module Function Arg1 Arg2 ...~n"),
    halt(1).

%% The escript runs as a separate OS process, so it has to put the compiled
%% pact_erlang beams on its own code path. The script is copied into the
%% application's priv dir, so `../ebin' relative to the script itself is
%% correct no matter what the current working directory is. Under Common Test
%% the cwd is the ct_run log directory, not the project root, which is why a
%% cwd based path alone does not work. The cwd based path is kept as a
%% fallback (honouring MIX_ENV) for setups that invoke the script from a
%% project root with a different layout.
add_pact_erlang_to_code_path() ->
    ScriptPrivDir = filename:dirname(filename:absname(escript:script_name())),
    ScriptRelativeEbin = filename:join(filename:dirname(ScriptPrivDir), "ebin"),
    {ok, Cwd} = file:get_cwd(),
    MixEnv = os:getenv("MIX_ENV", "test"),
    CwdRelativeEbin = filename:join([Cwd, "_build", MixEnv, "lib", "pact_erlang", "ebin"]),
    lists:foreach(
        fun(Path) -> code:add_pathz(Path) end,
        [ScriptRelativeEbin, CwdRelativeEbin]
    ).
