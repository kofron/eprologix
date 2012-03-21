-module(why).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% External API %%%
%%%%%%%%%%%%%%%%%%%%
-export([send/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal fsm state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{c,q}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal fsm record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(req,{addr,qs,res,sndr}).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([waiting/2]).
-export([set_address/2]).
-export([sending/2]).
-export([receiving/2]).
-export([finishing/2]).

%%%%%%%%%%%%%%
%%% Macros %%%
%%%%%%%%%%%%%%
-define(NOW,0).

send(_FSMId, GPIBAddr, QueryString) ->
	Req = #req{
		addr = GPIBAddr,
		qs = QueryString,
		res = none,
		sndr = none
	},
	gen_fsm:sync_send_all_state_event(?MODULE,Req).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	InitialState = #state{q = []},
	{ok, waiting, InitialState}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(R, From, waiting, StateData) when is_record(R,req) ->
	Rp = tag_req_from(R,From),
	NewStateData = StateData#state{
		c = Rp
	},
	{next_state, sleeping, NewStateData, ?NOW};
handle_sync_event(R, F, Else, #state{q=Q}=SD) when is_record(R,req)->
	Rp = tag_req_from(R,F),
	NewStateData = SD#state{
		q = Q ++ [Rp]
	},
{reply, ok, Else, NewStateData}.

handle_info(nothi, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm state definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(timeout, StateData) ->
	{next_state, set_address, StateData, ?NOW}.

set_address(timeout, StateData) ->
	{next_state, sending, StateData, ?NOW}.

sending(timeout, StateData) ->
	{next_state, finishing, StateData, ?NOW}.

receiving(timeout, #state{c=#req{sndr=S}}=StateData) ->
	RT = 25 + random:uniform(150),
	timer:sleep(RT),
	gen_fsm:reply(S,ok),
	{next_state, finishing, StateData, ?NOW}.

finishing(timeout, #state{q=Q}) ->
	Branch = case Q of
		[] ->
			{next_state, waiting, #state{q=Q}};
		[H|T] ->
			{next_state, sleeping, #state{q=T,c=H},?NOW}
	end,
	Branch.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_req_from(R,F) ->
	R#req{
		sndr = F
	}.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
oks(N) ->
	[ok || _ <- lists:seq(1,N)].

main_test() ->
	ok = application:start(why),
	app(),
	stress().

app() ->
	N = 10,
	?_assertEqual(oks(N),lists:map(fun(_) -> why:go() end, lists:seq(1,N))).

stress() ->
	F = fun(N) -> 
			lists:foreach(fun(_) -> 
								_ = why:go() 
							end, 
						lists:seq(1,N)) 
		end,
	{timeout, 60, ?_assertEqual(ok, lists:foreach(fun(_) -> 
									spawn(fun() -> 
										ok = F(1000) 
									end) 
								end, lists:seq(1,1000)))}. 	
-endif.