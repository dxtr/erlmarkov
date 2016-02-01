-module(erlmarkov_generator).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {weight = 0 :: integer(),
		suffix = orddict:new() :: [{_, _}]}).

init([]) ->
    State = #state{},
    {ok, State};
init([{_Word, Weight}]) ->
    State = #state{weight = Weight},
    {ok, State};
init([{_Word, Weight}|{_Suffixes}]) ->
    State = #state{weight=Weight},
    {ok, State}.

terminate(_,_) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
