%%% @doc The controller interface for eprologix ethernet devices.
%%%
%%% The ethernet eprologix device is modeled as a finite state machine
%%% which polls the devices it knows about continuously, inspecting 
%%% the state of the status register.  When it detects a non-zero value
%%% on a status register, it consults the callback module which is associated
%%% with the GPIB address which asserted SRQ to determine what to do next.
%%% @author Jared Kofron <jared.kofron@gmail.com>
-module(ep_control).
-behaviour(gen_fsm).
-vsn(0).

-export([start_link/1]).
-export([init/1, 
	 handle_event/3, 
	 handle_sync_event/4, 
	 handle_info/3,
         terminate/3, 
	 code_change/4]).

-export([add_instrument/3,
	 send_raw/3,
	 send_raw_sync/3,
	 send_raw_sync/4
	]).

-export([connecting/2, 
	 idle/2, 
	 polling/2,
	 service_req/2,
	 event_status/2,
	 msg_avail/2]).

-define(NOW, 1).

-record(state, {
	  ip :: inet:ip_address(),
	  port :: integer(),
	  stage,
	  handle,
	  instr,
	  next,
	  term = <<"\r\n">>
	 }).
-record(in, {
	  mod :: module(),
	  listening :: [pid()],
	  sync_listeners = []:: [term()]
	 }).

-spec start_link(term())->{ok,pid()} | ignore | {error,term()}.
start_link(Args) -> 
    {id, ID} = proplists:lookup(id, Args),
    gen_fsm:start_link({local, ID}, ?MODULE, Args, []).

init(Args) ->
    {IP, Port} = get_addr_from_args(Args),
    InitialState = #state{ip=IP, 
			  port=Port, 
			  instr=dict:new(),
			  next = []
			 },
    {ok, connecting, InitialState, ?NOW}.

get_addr_from_args(Args) ->
    {ip, IP} = proplists:lookup(ip, Args),
    {port, Prt} = proplists:lookup(port, Args),
    {IP, Prt}.

add_instrument(EproID, GPIBAddr, CallbackMod) ->
    gen_fsm:sync_send_all_state_event(EproID, {add, GPIBAddr, CallbackMod}).

send_raw(EproID, GPIBAddr, Data) ->
    gen_fsm:send_all_state_event(EproID, {send_raw, GPIBAddr, Data}).
send_raw_sync(EproID, GPIBAddr, Data) ->
    send_raw_sync(EproID, GPIBAddr, Data, 1000).
send_raw_sync(EproID, GPIBAddr, Data, Timeout) ->
    gen_fsm:sync_send_all_state_event(EproID, {send_raw, GPIBAddr, Data, Timeout}).

connecting(timeout, #state{ip=IP,port=P}=StData) ->
    case gen_tcp:connect(IP, P,[binary,{packet,0}]) of
	{ok, H} ->
	    {next_state, idle, StData#state{handle=H}};
	{error, E} ->
	    {stop, E, StData}
    end.
idle(_Event, StData) ->
    {stop, unimplemented, StData}.
polling(_Event, #state{next=[],instr=I}=StData) ->
    Addrs = dict:fetch_keys(I),
    {next_state, polling, StData#state{next=Addrs}, ?NOW};
polling(_Event, #state{handle=H,term=T,next=[N|_Rest]}=StData) ->
    ok = serial_poll(H,N,T),
    {next_state, polling, StData}.
service_req(_Event, #state{term=T,handle=H,instr=I,next=[N|_R]}=StateData) ->
    {next_state, service_req, StateData}.
event_status(_Event, #state{term=T,handle=H}=StateData) ->
    ToSend = <<"*ESR?">>,
    ok = send_epro_data(H, ToSend, T, true),
    {next_state, event_status, StateData}.
msg_avail(_Event, #state{term=T,handle=H,instr=I,next=[N|_R]}=StateData) ->
    M = get_callback_mod(I, N),
    ToSend = M:handle_mav(<<>>),
    ok = send_epro_data(H, ToSend, T),
    {next_state, msg_avail, StateData, 1000}.

serial_poll(Handle, Addr, Term) ->
    ok = set_epro_state(addr, Handle, Addr, Term),
    gen_tcp:send(Handle, [<<"++spoll">>, Term]).

handle_event({send_raw, Addr, Data}, AnyState, #state{instr=_I,handle=H,term=T}=State) -> 
    ok = set_epro_state(addr, H, Addr,T),
    ok = send_epro_data(H, Data,T),
    {next_state, AnyState, State, ?NOW}.

set_epro_state(addr, Handle, Addr,Term) ->
    gen_tcp:send(Handle, [<<"++addr ">>,erlang:integer_to_list(Addr),Term]).

send_epro_data(Handle, Data, Terminator) ->
    send_epro_data(Handle, Data, Terminator, false).
send_epro_data(Handle, Data, Terminator, Readback) ->
    gen_tcp:send(Handle, [Data,Terminator]),
    case Readback of
	true ->
	    gen_tcp:send(Handle, [<<"++read eoi">>, Terminator]);
	false ->
	    ok
    end.

handle_sync_event({add, GPIBAddr, Mod}, From, idle, #state{instr=I, handle=H, term=T}=StData) ->
    StdSREBits = ep_gpib:bits(stb),
    StdESBBits = ep_gpib:bits(esr),
    ExtraSREBits = Mod:bits(stb),
    SREBitmask = gen_bitmask(StdSREBits ++ ExtraSREBits),
    ESEBitmask = gen_bitmask(StdESBBits),
    SREReq = [<<"*SRE ">>, io_lib:format("~B",[SREBitmask])],
    ESRReq = [<<"*ESE ">>, io_lib:format("~B",[ESEBitmask])],
    ok = send_epro_data(H, SREReq, T),
    ok = send_epro_data(H, ESRReq, T),
    NewI = dict:store(GPIBAddr, #in{mod=Mod,listening=[From]}, I),
    {reply, ok, polling, StData#state{instr=NewI}, ?NOW};

handle_sync_event({add, GPIBAddr, Mod}, From, AnyState, #state{instr=I}=StData) ->
    NewI = dict:store(GPIBAddr, #in{mod=Mod,listening=From}, I),
    {reply, ok, AnyState, StData#state{instr=NewI}};

handle_sync_event({send_raw, Addr, Data, _TMO}, From, AnyState, #state{instr=I,
								 handle=H,
								 term=T}=State) ->
    NewInstrDict = dict:update(Addr, 
			       fun(#in{sync_listeners=S}=SL) ->
				       SL#in{sync_listeners=[From|S]}
			       end,
			       I),
    ok = set_epro_state(addr, H, Addr, T),
    ok = send_epro_data(H, Data, T),
    {next_state, AnyState, State#state{instr=NewInstrDict}, ?NOW}.

gen_bitmask(Bits) ->
    gen_bitmask_acc(Bits, 0).
gen_bitmask_acc([], Acc) ->
    Acc;
gen_bitmask_acc([{_BitName, Pos}|R], Acc) ->
    gen_bitmask_acc(R, Acc + (1 bsl Pos)).

handle_info({tcp, H, Dt}, polling, #state{next=[_N|R],handle=H}=StData) -> 
    PollResult = binary_to_integer(strip_terminator(Dt)),
    Decoded = ep_gpib:decode_stb(PollResult),
    {NextState, NextData, TmOut} = case Decoded of
				       [{rqs, false}, {esb, false}, {mav, false}] ->
					   {polling, StData#state{next=R}, ?NOW};
				       [{rqs, true}, {esb, true}, {mav, _}] ->
					   {polling, StData, ?NOW};
				       [{rqs, false}, {esb, true}, {mav, false}] ->
					   {event_status, StData, ?NOW};
				       [{rqs, false}, {esb, false}, {mav, true}] ->
					   {msg_avail, StData, ?NOW}
				   end,
    {next_state, NextState, NextData, TmOut};
handle_info({tcp, H, Dt}, event_status, #state{handle=H, 
					       instr=I, 
					       term=T,
					       next=[N|R]}=StData) ->
    ESB = parse_nr1(Dt),
    Decoded = ep_gpib:decode_esr(ESB),
    Mod = get_callback_mod(I, N),
    {NextState, ToSend} = case Mod:handle_byte(event_status, Decoded, nil) of
			      {send, Data} ->
				  {polling, Data};
			      ignore ->
				  io:format("WARNING: ignoring instrument...~n"),
				  {polling, <<"*CLS">>}
			  end,
    ok = send_epro_data(H, ToSend, T),
    {next_state, NextState, StData#state{next=R}, ?NOW};
handle_info({tcp, H, Dt}, msg_avail, #state{handle=H, 
					    instr=I,
					    next=[N|R]}=StData) ->
    PidList = get_pids_for_addr(I, N),
    NewDict = case get_sync_listener_and_update(I, N) of
		  [] ->
		      I;
		  {SyncListener, Dict} ->
		      gen_fsm:reply(SyncListener, Dt),
		      Dict
	      end,
    broadcast_data(PidList, Dt),
    {next_state, polling, StData#state{next=R,instr=NewDict}, ?NOW}.

get_sync_listener_and_update(InstrDict, Addr) ->
    {ok, I} = dict:find(Addr, InstrDict),
    case I#in.sync_listeners of
	[] ->
	    [];
	[H|_T] ->
	    NewDict = dict:update(Addr,
				  fun(#in{sync_listeners=[_S|R]}=SL) ->
					  SL#in{sync_listeners=R}
				  end,
				  InstrDict),
	    {H, NewDict}
    end.
    
strip_terminator(Bin) ->
    {Start, _} = binary:match(Bin, [<<"\r\n">>,<<"\n">>]),
    binary:part(Bin, {0, Start}).
binary_to_integer(Bin) ->
    erlang:list_to_integer(binary:bin_to_list(Bin)).
get_callback_mod(InstrDict, Addr) ->
    {ok, I} = dict:find(Addr, InstrDict),
    I#in.mod.
get_pids_for_addr(InstrDict, Addr) ->
    {ok, I} = dict:find(Addr, InstrDict),
    I#in.listening.
broadcast_data(PidList, Data) ->
    lists:foreach(fun({X,_Ref}) ->
			  X ! Data
		  end, PidList).
parse_nr1(Bin) ->
    [F,_] = binary:split(Bin, <<"\n">>),
    erlang:list_to_integer(binary:bin_to_list(F)).
			  
terminate(_Reason, _StName, _StData) -> ok.
code_change(_OldVsn, StName, StData, _Extra) -> {ok, StName, StData}.
