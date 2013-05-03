%%%% @doc The agilent 34970a switch/MUX unit
%%%% @author Jared Kofron <jared.kofron@gmail.com>
-module(agilent34970a).
-export([bits/1,
	 handle_rqs/1, 
	 handle_byte/3,
	 handle_mav/1]).

bits(stb) ->
    [
     {alarm_ready, 1}, 
     {errors_ready, 2}, 
     {bad_data, 3}, 
     {std_op, 7}
    ].

handle_rqs(Service) ->
    <<>>.

handle_byte(event_status, DecodedByte, _State) ->
    case proplists:get_value(cmd_err, DecodedByte) of
	true ->
	    {send, <<"SYST:ERR?">>};
	false ->
	    ignore
    end.

handle_mav(_MAV) ->
    [<<"++read eoi">>].


