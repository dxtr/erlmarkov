-module(erlmarkov_token_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 300,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [#{id => erlmarkov_token,
	       start => {erlmarkov_token, start_link, []},
	       restart => Restart,
	       shutdown => Shutdown,
	       type => Type}],
    Result = {SupFlags, Children},

    io:format("Started erlmarkov_token_sup (~p)~n", [self()]),
    {ok, Result}.

start_child(Token) ->
    supervisor:start_child(?SERVER, [Token]).

