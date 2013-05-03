
-module(eprologix_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Left = {
      left_box,
      {
	ep_control,
	start_link,
	[[{id,left_box},{ip,{10,0,0,4}}, {port, 1234}]]
      },
      permanent,
      5000,
      worker,
      [ep_control]
     },
    Children = [Left],
    {ok, { {one_for_one, 5, 10}, Children} }.

