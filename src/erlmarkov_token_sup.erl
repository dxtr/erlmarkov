-module(erlmarkov_token_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
		intensity => 100,
		period => 300},


    Children = [#{id => erlmarkov_token,
	       start => {erlmarkov_token, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker}],
    Result = {SupFlags, Children},
    io:format("Started ~p (~p)~n", [?MODULE, self()]),
    {ok, Result}.

start_child(Token) ->
    supervisor:start_child(?SERVER, [Token]).

