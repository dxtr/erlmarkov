-module(erlmarkov_generator_sup).
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
    Children = [#{id => erlmarkov_generator,
	       start => {erlmarkov_generator, start_link, []},
	       restart => Restart,
	       shutdown => Shutdown,
	       type => Type}],
    Result = {SupFlags, Children},
    
    {ok, Result}.

start_child(Word) when is_atom(Word) ->
    supervisor:start_child(?SERVER, [Word]).
