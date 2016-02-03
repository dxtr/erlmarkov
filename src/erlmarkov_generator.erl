%% TODO: Move the chain generation here
%% TODO: Implement min/max length of chains

-module(erlmarkov_generator).
-behaviour(gen_server).
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 train_file/1,
	 train_chain/1,
	 get_chain/0,
	 get_chain/1,
	 start_link/0]).

-define(SERVER, ?MODULE).
-record(state, {min_chain_length = 3 :: integer(),
		max_chain_length = 15 :: integer()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

init([]) ->
    State = #state{},
    {ok, State};
init([Min, Max]) when is_integer(Min),
		      is_integer(Max),
		      Min > 0,
		      Min =< Max ->
    State = #state{min_chain_length = Min, max_chain_length = Max},
    {ok, State}.

terminate(_,_) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

train_file(File) ->
    {ok, Content} = file:read_file(File),
    ReplacedContent = re:replace(string:to_lower(binary_to_list(Content)),
				 "[\r\n:\(\)\?]", " ",
				 [global, {newline, anycrlf}, {return, binary}]),
    SplitContent = re:split(ReplacedContent, "\s+", [trim]),
    train_chain(SplitContent).

token_separator() -> " ".
%%chain_length(Chain) -> length(Chain)/2.

%% re:split(re:replace(list_to_binary(string:to_lower(binary_to_list(Data))), "[\n:\(\)\?]", " ", [global, {newline, anycrlf}, {return, binary}]), "\s+", [trim])]).
train_chain(Chain) ->
    erlmarkov_token:add_chain(Chain).

get_random_token() ->
    TokenProcMap = erlmarkov_proc_registry:dump_state(),
    TokenProcList = maps:keys(TokenProcMap),
    TokenCount = length(TokenProcList),
    Rand = rand:uniform(TokenCount),
    lists:nth(Rand, TokenProcList).

%% Function to get a random chain
get_chain() ->
    Token = get_random_token(),
    get_chain(Token).

get_chain(Start) ->
    gen_server:call(?SERVER, {get_chain, Start}).

get_chain([],
	  Accum,
	  #state{min_chain_length = MinChainLength,
		 max_chain_length = MaxChainLength} = _State)
  when is_list(Accum),
       MinChainLength > 0,
       MaxChainLength >= MinChainLength,
       length(Accum) > MinChainLength*2 ->
    lists:flatten(Accum);
get_chain(Start,
	  Accum,
	  #state{min_chain_length = MinChainLength,
		 max_chain_length = MaxChainLength} = State)
  when is_list(Accum),
       MinChainLength > 0,
       MaxChainLength >= MinChainLength ->
    ChainLength = length(Accum),
    if ChainLength =< MaxChainLength*2 ->
	    case erlmarkov_proc_registry:get_pid(Start) of
		undefined ->
		    io:format("Could not find process for ~p~n", [Start]),
		    [];
		Proc ->
		    NextToken = gen_server:call(Proc, {next_token}),
		    A = if Accum =:= [] -> [Start];
			   true -> Accum ++ [token_separator()] ++ [Start]
			end,
		    get_chain(NextToken, A, State)
	    end;
       true -> get_chain([], Accum, State)
    end.

handle_call({train_file, File}, _From, State) ->
    train_file(File),
    {reply, ok, State};
handle_call({get_chain}, _From, State) ->
    Reply = get_chain(),
    {reply, Reply, State};
handle_call({get_chain, Start}, _From, State) ->
    Reply = get_chain(Start, [], State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
