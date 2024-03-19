-module(my_server).
-behaviour(gen_server).

-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
    start_server/0, stop_server/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

start_server() ->
    gen_server:cast(?MODULE, {start_api_server}).

stop_server() ->
    gen_server:cast(?MODULE, {stop_api_server}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_api_server}, State) ->
    Dispatch = cowboy_router:compile([
        {<<"127.0.0.1">>, [
            {"/test_weather/[...]", test_weather_api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        test_weather_api_handler,
        [{port, 8080}], 
        #{env => #{dispatch => Dispatch}}
    ),
    Dispatch,
    {noreply, State};

handle_cast({stop_api_server}, State) ->
    ok = cowboy:stop_listener(test_weather_api_handler),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
