%%%-------------------------------------------------------------------
%% @doc erlmarkov top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('erlmarkov_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 300,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [erlmarkov_proc_registry(),
		erlmarkov_token_supervisor(),
		erlmarkov_generator_supervisor()],
    Result = {SupFlags, Children},

    {ok, Result}.

%%====================================================================
%% Internal functions
%%====================================================================
erlmarkov_proc_registry() ->
    #{id => erlmarkov_proc_registry,
     start => {erlmarkov_proc_registry, start_link, []},
     restart => permanent,
     shutdown => 2000,
     type => worker,
     modules => [erlmarkov_proc_registry]}.

erlmarkov_token_supervisor() ->
    #{id => erlmarkov_token_sup,
     start => {erlmarkov_token_sup, start_link, []},
     restart => permanent,
     shutdown => 2000,
     type => supervisor,
     modules => [erlmarkov_token_sup]}.

erlmarkov_generator_supervisor() ->
    #{id => erlmarkov_generator_sup,
     start => {erlmarkov_generator_sup, start_link, []},
     restart => permanent,
     shutdown => 2000,
     type => supervisor,
     modules => [erlmarkov_generator_sup]}.
