%% TODO: Implement some kind of ping functionality, if neccessary
%% TODO: Implement some kind of ACL so processes can't override each other

-module(erlmarkov_proc_registry).
-behaviour(gen_server).
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 start_link/0,
	 register_name/1,
	 register_name/2,
	 get_pid/1,
	 dump_state/0]).
-define(SERVER, ?MODULE).

register_name(Name) ->
    register_name(Name, self()).

register_name(Name, Pid) when is_pid(Pid) ->
    gen_server:cast(whereis(?SERVER), {register, Name, Pid}).

get_pid(Name) ->
    gen_server:call(whereis(?SERVER), {whereis, Name}).

dump_state() ->
    gen_server:call(whereis(?SERVER), {dump_state}).

init([]) ->	 
    State = maps:new(),
    io:format("Started erlmarkov_proc_registry (~p)~n", [self()]),
    {ok, State}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(timeout, State) ->
    {noreply, State}.

handle_cast({register, Name, Pid}, State) when is_pid(Pid) ->
    NewState = do_register(Name, Pid, State),
    {noreply, NewState};
handle_cast({unregister, Name, Pid}, State) when is_pid(Pid) ->
    NewState = do_unregister(Name, Pid, State),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({register, Name}, {Pid, _Tag}, State) ->
    NewState = do_register(Name, Pid, State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({unregister, Name}, {Pid, _Tag}, State) ->
    NewState = do_unregister(Name, Pid, State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({whereis, Name}, _From, State) ->
    Reply = maps:get(Name, State, undefined),
    {reply, Reply, State};
handle_call({dump_state}, _From, State) ->
    Reply = State,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

do_register(Name, Pid, State) when is_pid(Pid) ->
    io:format("Registering ~p~n", [Name]),
    maps:put(Name, Pid, State).

do_unregister(Name, Pid, State) when is_pid(Pid) ->
    io:format("Unregistering ~p~n", [Name]),
    maps:remove(Name, State).
