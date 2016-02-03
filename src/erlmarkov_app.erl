%%%-------------------------------------------------------------------
%% @doc erlmarkov public API
%% @end
%%%-------------------------------------------------------------------

-module('erlmarkov_app').

-behaviour(application).

%% Application callbacks
-export([start/0,
	 start/2,
	 stop/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    start([],[]).

start(_StartType, _StartArgs) ->
    'erlmarkov_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================