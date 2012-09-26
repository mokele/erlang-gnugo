-module(gnugo_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([
    start_link/0,
    start_child/0
  ]).

%% Supervisor callbacks
-export([
    init/1
  ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?SERVER, [self()]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
        {undefined, {gnugo, start_link, []},
          temporary, brutal_kill, supervisor,
          [gnugo]}
      ]} }.
