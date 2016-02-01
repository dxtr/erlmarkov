-module(erlmarkov_token).
-behaviour(gen_server).
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 add_chain/1,
	 start_link/1,
	 get_chain/0,
	 get_chain/1]).

-record(state, {token,
		weight = 0 :: integer(),
		suffix = [] :: [{term(), integer()}]}).

start_link(Token) ->
    gen_server:start_link(?MODULE, [Token], []).

init([Token]) ->
    State = #state{token = Token},
    gen_server:cast(whereis(erlmarkov_proc_registry),
		    {register, Token, self()}),
    {ok, State}.

find_suffix(#state{weight = _StateWeight, suffix = Suffixlist} = _State, Suffix) ->
    case lists:keysearch(Suffix, 1, Suffixlist) of
	{value, {Suffix, Weight}} -> {Suffix, Weight};
	false -> {Suffix, 0}
    end.

add_suffix(#state{weight = OldWeight, suffix = OldSuffix} = State, Suffix) ->
    NewWeight = OldWeight + 1,
    {S, W} = find_suffix(State, Suffix),
    NewSuffix = lists:keystore(Suffix, 1, OldSuffix, {S, W + 1}),
    #state{weight = NewWeight, suffix = NewSuffix}.

add_suffix_list(#state{} = State, [Suffix]) ->
    NewState = add_suffix(State, hd(Suffix)),
    add_suffix_list(NewState, tl(Suffix));
add_suffix_list(#state{} = State, []) ->
    State.

add_chain([Token|Suffix]) ->
    Pid = find_token(Token),
    case Suffix of
	[] -> gen_server:cast(Pid, {add_suffix, []});
	_ -> gen_server:cast(Pid, {add_suffix, hd(Suffix)}),
	     add_chain(Suffix)
    end;
add_chain([]) ->
    ok.

get_random_token() ->
    TokenProcMap = erlmarkov_proc_registry:dump_state(),
    TokenProcList = maps:keys(TokenProcMap),
    TokenCount = length(TokenProcList),
    Rand = rand:uniform(TokenCount),
    lists:nth(Rand, TokenProcList).

token_separator() -> " ".

%% Function to get a random chain
get_chain() ->
    Token = get_random_token(),
    get_chain(Token).

get_chain(Start) ->
    get_chain(Start, []).

get_chain([], Accum) ->
    lists:flatten(Accum);
get_chain(Start, Accum) ->
    case erlmarkov_proc_registry:get_pid(Start) of
	undefined ->
	    io:format("Could not find process for ~p~n", [Start]);
	Proc ->
	    NextToken = gen_server:call(Proc, {next_token}),
	    A = if Accum =:= [] -> [Start];
		   true -> [Accum, token_separator(), Start]
		end,
	    get_chain(NextToken, A)
    end.    

next_token(#state{weight = Weight, suffix = Suffix} = _State) ->
    Rand = rand:uniform(Weight),
    next_token(Suffix, Rand).

next_token([Suffix|Rest], Acc) when Acc > 0 ->
    {Token, Weight} = Suffix,
    NewAcc = Acc - Weight,
    if NewAcc > 0 -> next_token(Rest, NewAcc);
       true -> Token
    end;
next_token([Suffix], Acc) when Acc =< 0 ->
    hd(Suffix);
next_token(_, _) ->
    [].

find_token(Token) ->
    case erlmarkov_proc_registry:get_pid(Token) of
	undefined -> register_token(Token);
	Pid when is_pid(Pid) -> Pid
    end.

register_token(Token) ->
    case erlmarkov_token_sup:start_child(Token) of
	{ok, Pid} -> Pid;
	{error, Error} ->
	    io:format("Error: ~p~n", [Error]),
	    {error}
    end.

terminate(_Reason, #state{token = Token} = _State) ->
    gen_server:cast(whereis(erlmarkov_proc_registry),
			    {unregister, Token, self()}),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

handle_call({add_suffix, Suffix}, _From, State) ->
    NewState = add_suffix(State, Suffix),
    Reply = NewState,
    {reply, Reply, NewState};
handle_call({add_suffix_list, [Suffix]}, _From, State) ->
    NewState = add_suffix_list(State, Suffix),
    Reply = NewState,
    {reply, Reply, NewState};
handle_call({next_token}, _From, State) ->
    Reply = next_token(State),
    {reply, Reply, State};
handle_call({get_chain}, _From, State) ->
    Reply = get_chain(),
    {reply, Reply, State};
handle_call({get_chain, Start}, _From, State) ->
    Reply = get_chain(Start),
    {reply, Reply, State};
handle_call({dump_state}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_suffix, Suffix}, State) ->
    NewState = add_suffix(State, Suffix),
    {noreply, NewState};
handle_cast({add_suffix_list, [Suffix]}, State) ->
    NewState = add_suffix_list(State, Suffix),
    {noreply, NewState}.

handle_info(timeout, State) ->
    {noreply, State}.
