-module(eprologix_cmdr).
-behavior(gen_fsm).

-record(state,{ip_addr, port, c_sock, c_req}).
-record(request,{type,from,data,tag}).

%% API
-export([start_link/0]).
-export([configuring/2,waiting/2,waiting/3,sending/2,receiving/2,parse_reply/2,finishing/2]).
-export([send_query/1, send_command/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
send_query(QueryString) ->
	Req = #request{
		type = q,
		from = self(), 
		data = QueryString, 
		tag = make_ref()
	},
	gen_fsm:send_all_state_event(?MODULE,Req).

send_command(CommandString) ->
	Req = #request{
		type = c,
		from = self(),
		data = CommandString,
		tag = make_ref()
	},
	gen_fsm:send_all_state_event(?MODULE,Req).

%%%%%%%%%%%%%%%
%%% GEN_FSM %%%
%%%%%%%%%%%%%%%
start_link() ->
	start_link({10,0,0,4},1234).
start_link(IPAddr,PortNumber) ->
	gen_fsm:start_link({local,?MODULE},?MODULE,[IPAddr,PortNumber],[]).

init([IPAddr,Port]) ->
	case get_telnet_sock(IPAddr,Port) of
		{ok, _} ->
			St = #state{ip_addr = IPAddr, port = Port},
			{ok, configuring, St, 0};
		{error, _}=E ->
			E
	end.

handle_event(Ev, waiting, #state{ip_addr = A, port = P} = StateData) 
		when is_record(Ev, request) ->
	{ok, Socket} = get_telnet_sock(A,P),
	NewState = StateData#state{c_sock = Socket, c_req = Ev},
	{next_state, sending, NewState, 0}.

handle_sync_event(Event, _From, StateName, StateData) ->
	io:format("<~p>~n",Event),
	{reply, got_query, StateName, StateData}.

handle_info({tcp,S,Data}=R, receiving, #state{c_sock=S}=StateData) ->
	gen_fsm:send_event(?MODULE,R),
	{next_state, receiving, StateData};
handle_info({tcp_closed,S}, StateName, #state{c_sock=S}=StateData) ->
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

receiving({tcp,S,Data}, #state{c_sock=S}=StateData) ->
	io:format("got reply:~p~n",[Data]),
	{next_state, parse_reply, StateData, 0};
receiving(timeout, #state{c_sock=S}=StateData) ->
	gen_tcp:send(S,"++read eoi\n"),
	{next_state, receiving, StateData}.

parse_reply(timeout, StateData) ->
	{next_state, finishing, StateData, 0}.

finishing(timeout, #state{c_sock=S}=StateData) ->
	close_telnet_sock(S),
	io:format("completed loop.  waiting again...~n"),
	NewStateData = StateData#state{c_sock = none, c_req = none},
	{next_state, waiting, NewStateData}.

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