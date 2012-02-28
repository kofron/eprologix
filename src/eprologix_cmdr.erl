-module(eprologix_cmdr).
-behavior(gen_fsm).

-record(state,{id, ip_addr, port, c_sock, c_req, req_q}).
-record(request,{type,from,data,tag,result}).

%% API
-export([start_link/3]).
-export([configuring/2,waiting/2,waiting/3,sending/2,receiving/2,send_reply/2,finishing/2]).
-export([send_query/2, send_command/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
send_query(FSMId, QueryString) ->
	T = make_ref(),
	Req = #request{
		type = q,
		from = self(), 
		data = QueryString, 
		tag = T
	},
	gen_fsm:send_all_state_event(FSMId,Req),
	{ok,T}.

send_command(FSMId, CommandString) ->
	T = make_ref(),
	Req = #request{
		type = c,
		from = self(),
		data = CommandString,
		tag = T
	},
	gen_fsm:send_all_state_event(FSMId,Req),
	{ok,T}.

%%%%%%%%%%%%%%%
%%% GEN_FSM %%%
%%%%%%%%%%%%%%%
start_link(IPAddr,PortNumber,FSMId) ->
	gen_fsm:start_link({local,FSMId},?MODULE,[IPAddr,PortNumber],[]).

init([IPAddr,Port]) ->
	case get_telnet_sock(IPAddr,Port) of
		{ok, _} ->
			St = #state{ip_addr = IPAddr, port = Port, req_q = queue:new()},
			{ok, configuring, St, 0};
		{error, _}=E ->
			E
	end.

handle_event(Ev, waiting, #state{ip_addr = A, port = P} = StateData) 
		when is_record(Ev, request) ->
	{ok, Socket} = get_telnet_sock(A,P),
	NewState = StateData#state{c_sock = Socket, c_req = Ev},
	{next_state, sending, NewState, 0};
handle_event(Ev, AnyState, #state{req_q=Q} = StateData) 
		when is_record(Ev, request) ->
	NewQ = push_ev(Ev,Q),
	NewState = StateData#state{req_q = NewQ},
	{next_state, AnyState, NewState}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	{stop, unhandled_sync_event, StateData}.

handle_info({tcp,S,_Data}=R, receiving, #state{c_sock=S}=StateData) ->
	gen_fsm:send_event(?MODULE,R),
	{next_state, receiving, StateData};
handle_info({tcp_closed,S}, _StateName, #state{c_sock=S}=StateData) ->
	{stop, socket_abort, StateData};
handle_info({tcp_closed,_}, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%
%%% STATES %%%
%%%%%%%%%%%%%%
configuring(timeout, #state{ip_addr=A,port=P}=StateData) ->
	{ok, Sock} = get_telnet_sock(A,P),
	S = default_conf_string(),
	send_telnet_string(Sock,S),
	close_telnet_sock(Sock),
	{next_state,waiting,StateData}.

waiting(_Event,StateData) ->
	{next_state, waiting, StateData}.
waiting(_Event,_From,StateData) ->
	{reply, ok, waiting, StateData}.

sending(timeout, #state{c_req=R,c_sock=S}=StateData) ->
	ToSend = case is_newline_terminated(R#request.data) of
		true ->
			R#request.data;
		false ->
			[R#request.data|"\n"]
		end,
	NextState = case should_wait_for_reply(R) of
		true ->
			receiving;
		false ->
			finishing
		end,
	gen_tcp:send(S,ToSend),
	{next_state, NextState, StateData,0}.

receiving({tcp,S,Data}, #state{c_sock=S,c_req=R}=StateData) ->
	NewRequest = R#request{result=Data},
	NewStateData = StateData#state{c_req = NewRequest},
	{next_state, send_reply, NewStateData, 0};
receiving(no_response, #state{c_req=R}=StateData) ->
	NewReq = R#request{result={error, no_response}},
	NewStateData = StateData#state{c_req = NewReq},
	{next_state, send_reply, NewStateData, 0};	
receiving(timeout, #state{c_sock=S}=StateData) ->
	gen_tcp:send(S,"++read eoi\n"),
	gen_fsm:send_event_after(1000,no_response),
	{next_state, receiving, StateData}.

send_reply(timeout, #state{c_req=#request{from=F,result=R,tag=T}}=StateData) ->
	gen_fsm:reply({F,T},R),
	{next_state, finishing, StateData, 0}.

finishing(timeout, #state{c_sock=S,req_q=Q}=StateData) ->
	{NewStateData, NextState, Timeout} = case next_request(Q) of
			empty ->
				close_telnet_sock(S),
				NData = StateData#state{c_sock = none, c_req = none},
				NState = waiting,
				{NData,NState, infinity};
			{next, Ev, NewQ} ->
				NData = StateData#state{c_req = Ev, req_q = NewQ},
				NState = sending,
				{NData,NState, 0}
	end,

	{next_state, NextState, NewStateData, Timeout}.

%%%%%%%%%%%%%%%%
%%% INTERNAL %%%
%%%%%%%%%%%%%%%%
get_telnet_sock(IPAddr,Port) ->
	Ret = case gen_tcp:connect(IPAddr,Port,[binary,{packet,0}]) of
		{ok, _}=Success ->	
			Success;
		{error, _}=Failure ->
			Failure
	end,
	Ret.

close_telnet_sock(Socket) ->
	ok = gen_tcp:close(Socket).

send_telnet_string(Socket,String) ->
	String0 = case is_newline_terminated(String) of
		true ->
			String;
		false ->
			[String|"\n"]
	end,
	gen_tcp:send(Socket,String0).

is_newline_terminated(String) ->
	lists:prefix("\n",lists:reverse(String)).

default_conf_string() ->
	"++clr\n++mode 1\n++auto 0\n++eos 0\n++eoi 1\n".

should_wait_for_reply(#request{type = c}) ->
	false;
should_wait_for_reply(#request{type = q}) ->
	true.

push_ev(Event,Queue) ->
	queue:in(Event,Queue).
next_request(Queue) ->
	case queue:out(Queue) of
		{{value, Event}, NewQ} ->
			{next, Event, NewQ};
		{empty, _OldQ} ->
			empty
	end.