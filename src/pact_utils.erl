-module(pact_utils).

-export([
    run_executable_async/1,
    run_executable_async/2
]).

-spec run_executable_async(string()) -> {integer(), string()}.
run_executable_async(Cmd) ->
    EscapedCmd = escape_special_chars(Cmd),
    Port = erlang:open_port({spawn, EscapedCmd}, [stream, in, eof, hide, exit_status]),
    get_data_from_executable(Port, []).

%% @doc Runs an executable with an explicit list of arguments.
%% Every argument is quoted individually, so arguments that are empty or
%% contain spaces are passed through to the executable as-is instead of being
%% dropped or split by the shell.
-spec run_executable_async(string(), [string()]) -> {integer(), string()}.
run_executable_async(Executable, Args) ->
    Cmd = string:join([quote_part(Part) || Part <- [Executable | Args]], " "),
    Port = erlang:open_port({spawn, Cmd}, [stream, in, eof, hide, exit_status]),
    get_data_from_executable(Port, []).

escape_special_chars(Cmd) ->
    %% Split the command into parts and escape them
    Parts = string:tokens(Cmd, " "),
    EscapedParts = lists:map(fun escape_part/1, Parts),
    string:join(EscapedParts, " ").

escape_part(Part) ->
    %% Check if the part contains special characters and escape it
    case re:run(Part, "^[\\w\\d\\-]+$") of
        {match, _} -> Part; %% If it matches a simple word, return as is
        nomatch -> quote_part(Part)
    end.

quote_part(Part) ->
    "'" ++ re:replace(Part, "'", "'\\''", [global, {return, list}]) ++ "'".

get_data_from_executable(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data_from_executable(Port, [Sofar | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT', Port, _} -> ok
            after
                % force context switch
                1 -> ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Sofar)}
    end.
