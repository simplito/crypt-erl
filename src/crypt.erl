-module(crypt).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([gensalt/1, gensalt/2, crypt/2]).
-export([change_workers_limits/2]).

-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-define(APPNAME, crypt).
-define(CMDNAME, crypt_helper).

-record(state, {
  worker_path = worker_path(),
  workers     = #{}, 
  freelist    = [],
  waiting     = queue:new(),
  min_workers = application:get_env(?APPNAME, min_workers, 0),
  max_workers = application:get_env(?APPNAME, max_workers, 16)
}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

gensalt(Prefix) ->
    gensalt(Prefix, 0).

gensalt(Prefix, Rounds) when is_list(Prefix), is_integer(Rounds) ->
    execute("gensalt" ++ "\n" ++  Prefix ++ "\n" ++ integer_to_list(Rounds) ++ "\n").

crypt(Phrase, Salt) when is_list(Phrase), is_list(Salt) ->
    execute("crypt" ++ "\n" ++ Phrase ++"\n" ++ Salt ++ "\n").

change_workers_limits(MinWorkers, MaxWorkers) when is_integer(MinWorkers), is_integer(MaxWorkers), MinWorkers >= 0, MinWorkers =< MaxWorkers->
    gen_server:call(?MODULE, {change_workers_limits, MinWorkers, MaxWorkers}).

% priv
execute(Cmd) ->
    gen_server:call(?MODULE, {execute, Cmd}).

%%====================================================================
%% gen_server behaviour functions
%%====================================================================

init([]) ->
    {ok, start_free_workers(#state{})}.

handle_call({execute, Cmd}, From, #state{ freelist = [_|_] } = State) ->
    {noreply, schedule(Cmd, From, State)};

handle_call({execute, Cmd}, From, #state{ workers = Workers, freelist = [], max_workers = MaxWorkers } = State) ->
    case maps:size(Workers) >= MaxWorkers of
        true  -> {noreply, enqueue(Cmd, From, State)};
        false -> {noreply, schedule(Cmd, From, start_worker(State))}
    end;

handle_call({change_workers_limits, MinWorkers, MaxWorkers}, _From, State) ->
    {reply, ok, stop_free_workers(start_free_workers(State#state{ min_workers = MinWorkers, max_workers = MaxWorkers }))}.


handle_cast(_, State) ->
    {noreply, State}.


handle_info({Port, _Info}, #state{ workers = Workers } = State) when is_port(Port), not is_map_key(Port, Workers) ->
    % ignore spurious messages for already closed workers
    {noreply, State};

handle_info({Port, {data, {eol, Response}}}, #state{ workers = Workers } = State) when is_port(Port) ->
    case Workers of
        #{ Port := {_Cmd, From} } -> 
            case Response of
                [] -> gen_server:reply(From, {error, badarg});
                _  -> gen_server:reply(From, {ok, Response})
            end,
            {noreply, stop_free_workers(dequeue(add_worker(Port, State)))};
        #{ Port := ready } ->
            ?LOG_ERROR("unexpected response: ~p ~p~n", [Port, Response]),
            {noreply, State}
    end;

handle_info({Port, {exit_status, Rc}}, State) when is_port(Port) ->
    {noreply, handle_down_worker(Port, {exit, Rc}, State)};

handle_info({'DOWN', _Ref, port, Port, Reason},  State) ->
    {noreply, handle_down_worker(Port, {down, Reason}, State)};

handle_info(Message, State) ->
    ?LOG_ERROR("unhandled: ~p~n", [Message]),
    {noreply, State}.

terminate(_, State) ->
    lists:foldl(fun stop_worker/2, State, maps:keys(State#state.workers)),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

add_worker(Port, #state{ workers = Workers, freelist = Freelist } = State) ->
    State#state{ workers = Workers#{ Port => ready }, freelist = [Port | Freelist]}.

remove_worker(Port, #state{ workers = Workers, freelist = Freelist } = State) ->
    State#state{ workers = maps:remove(Port, Workers), freelist = Freelist -- [Port] }.

schedule(Cmd, From, #state{ workers = Workers, freelist = [FreeWorker|Rest] } = State) ->
    port_command(FreeWorker, Cmd),
    State#state{ workers = Workers#{ FreeWorker => {Cmd, From} }, freelist = Rest }.

enqueue(Cmd, From, #state{ freelist = [_|_] } = State) ->
    schedule(Cmd, From, State);
enqueue(Cmd, From, #state{ waiting = Queue } = State) ->
    NewQueue = queue:in({Cmd, From}, Queue),
    State#state{ waiting = NewQueue }.

dequeue(#state{ freelist = [] } = State) ->
    State;
dequeue(#state{ waiting = Queue } = State) ->
    case queue:out(Queue) of
        {empty, _} -> 
            State;
        {{value, {Cmd,From}}, NewQueue} -> 
            schedule(Cmd, From, State#state{ waiting = NewQueue })
    end.

start_worker(#state{ worker_path = WorkerPath } = State) ->
    Port = erlang:open_port({spawn, WorkerPath}, [{line, 1024}, exit_status]),
    monitor(port, Port),
    add_worker(Port, State).

stop_worker(Port, State) ->
    Port ! {self(), close},
    remove_worker(Port, State).

start_free_workers(#state{ workers = Workers, min_workers = MinWorkers } = State) when map_size(Workers) < MinWorkers ->
    start_free_workers(start_worker(State));
start_free_workers(State) ->
    State.

stop_free_workers(#state{ freelist = [FirstFree|_Rest], workers = Workers, min_workers = MinWorkers } = State) when map_size(Workers) > MinWorkers ->
    stop_free_workers(stop_worker(FirstFree, State));
stop_free_workers(State) ->
    State.

handle_down_worker(Port, _Reason, #state{ workers = Workers } = State) when not is_map_key(Port, Workers) ->
    State;
handle_down_worker(Port, Reason, #state{ workers = Workers } = State) ->
    case Workers of
        #{ Port := {_Cmd, From} } -> gen_server:reply(From, {error, Reason});
        #{ Port := ready } -> ok
    end,
    NewState = remove_worker(Port, State),
    case queue:is_empty(State#state.waiting) of
        true  -> start_free_workers(NewState);
        false -> dequeue(start_worker(NewState))
    end.

% utility function
worker_path() ->
    case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?CMDNAME]);
                _ ->
                    filename:join([priv, ?CMDNAME])
            end;
        Dir ->
            filename:join(Dir, ?CMDNAME)
    end.