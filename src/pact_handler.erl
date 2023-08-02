-module(pact_handler).
-behaviour(gen_server).

-export([
    start_pact/2,
    create_interaction/3, get_interaction/1,
    set_mock_server_port/2, get_mock_server_port/1,
    stop/1,
    get_pact_ref/1, set_pact_ref/2,
    get_consumer_producer/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(pact_state, {
    consumer,
    producer,
    pact_ref = undefined,
    interaction = {undefined, #{}},
    mock_server_port = undefined
}).

% Public API to start the server
start_pact(Consumer, Producer) ->
    gen_server:start({global, {?MODULE, Consumer, Producer}}, ?MODULE,
        #pact_state{
            consumer = Consumer,
            producer = Producer
        },
        []
    ).

get_consumer_producer(PactPid) ->
    gen_server:call(PactPid, get_consumer_producer).

get_pact_ref(PactPid) ->
    gen_server:call(PactPid, get_pact_ref).

set_pact_ref(PactPid, PactRef) ->
    gen_server:call(PactPid, {set_pact_ref, PactRef}).

% Public API to create an interaction
create_interaction(PactPid, InteractionRef, Interaction) ->
    gen_server:call(PactPid, {create_interaction, InteractionRef, Interaction}).

% Public API to retrieve all interactions stored in the state
get_interaction(PactPid) ->
    gen_server:call(PactPid, get_interaction).

% Public API to set the mock server port when the server is started
set_mock_server_port(PactPid, Port) ->
    gen_server:call(PactPid, {set_mock_server_port, Port}).

% Public API to set the mock server port when the server is started
get_mock_server_port(PactPid) ->
    gen_server:call(PactPid, get_mock_server_port).


%% gen_server callbacks

init(#pact_state{consumer = Consumer, producer = Producer}) ->
    {ok, #pact_state{consumer = Consumer, producer = Producer}}.

handle_call({create_interaction, InteractionRef, Interaction}, _From, State) ->
    NewState = State#pact_state{interaction = {InteractionRef, Interaction}},
    {reply, ok, NewState};

handle_call(get_interaction, _From, State) ->
    {reply, State#pact_state.interaction, State};

handle_call({set_mock_server_port, Port}, _From, State) ->
    NewState = State#pact_state{mock_server_port=Port},
    {reply, ok, NewState};

handle_call(get_mock_server_port, _From, State) ->
    {reply, State#pact_state.mock_server_port, State};

handle_call(get_pact_ref, _From, State) ->
    {reply, State#pact_state.pact_ref, State};

handle_call({set_pact_ref, PactRef}, _From, State) ->
    NewState = State#pact_state{pact_ref=PactRef},
    {reply, ok, NewState};

handle_call(get_consumer_producer, _From, State) ->
    {reply, {State#pact_state.consumer, State#pact_state.producer}, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop(PactPid) ->
    gen_server:stop(PactPid).
