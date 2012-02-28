
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
	LeftCmdr = {
		left_box,
		{
			eprologix_cmdr,
			start_link,
			[{10,0,0,4},1234,left_box]
		},
		permanent,
		5000,
		worker,
		[eprologix_cmdr]
	},
	RightCmdr = {
		right_box,
		{
			eprologix_cmdr,
			start_link,
			[{10,0,0,3},1234,right_box]
		},
		permanent,
		5000,
		worker,
		[eprologix_cmdr]
	},
    {ok, { {one_for_one, 5, 10}, [LeftCmdr,RightCmdr]} }.

