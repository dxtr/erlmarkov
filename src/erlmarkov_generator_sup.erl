-module(erlmarkov_generator_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/0, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
		intensity => 100,
		period => 300},

    Children = [#{id => erlmarkov_generator,
	       start => {erlmarkov_generator, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker}],
    Result = {SupFlags, Children},
    io:format("Started ~p (~p)~n", [?MODULE, self()]),
    {ok, Result}.

start_child() ->
    supervisor:start_child(?SERVER, []).
