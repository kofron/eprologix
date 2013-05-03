%%% @doc functions for decoding various GPIB things
%%% @author Jared Kofron <jared.kofron@gmail.com>
-module(ep_gpib).
-export([bits/1, decode_stb/1, decode_esr/1]).

-define(rqs_bit, 6).
-define(esb_bit, 5).
-define(mav_bit, 4).
-define(pwr_on_bit, 7).
-define(usr_req_bit, 6).
-define(cmd_err_bit, 5).
-define(exec_err_bit, 4).
-define(dev_err_bit, 3).
-define(qry_err_bit, 2).
-define(req_ctrl_bit, 1).
-define(op_compl_bit, 0).

bits(stb) ->
    [
     {event_summary, ?esb_bit},
     {msg_avail, ?mav_bit}
    ];
bits(esr) ->
    [
     {op_comp, ?op_compl_bit},
     {qry_err, ?qry_err_bit},
     {dev_err, ?dev_err_bit},
     {exec_err, ?exec_err_bit},
     {cmd_err, ?cmd_err_bit},
     {pwr_on, ?pwr_on_bit}
    ].


decode_stb(Byte) when is_integer(Byte) ->
    [
     {rqs, bit_get(Byte,?rqs_bit)},
     {esb, bit_get(Byte,?esb_bit)},
     {mav, bit_get(Byte,?mav_bit)}
    ].

bit_get(0, _) ->
    false;
bit_get(Byte, Pos) ->
    (Byte band (1 bsl Pos)) == Byte.

decode_esr(Byte) when is_integer(Byte) ->
    [
     {pwr_on, bit_get(Byte, ?pwr_on_bit)},
     {usr_req, bit_get(Byte, ?usr_req_bit)},
     {cmd_err, bit_get(Byte, ?cmd_err_bit)},
     {exec_err, bit_get(Byte, ?exec_err_bit)},
     {dev_err, bit_get(Byte, ?dev_err_bit)},
     {qry_err, bit_get(Byte, ?qry_err_bit)},
     {req_ctrl, bit_get(Byte, ?req_ctrl_bit)},
     {op_compl, bit_get(Byte, ?op_compl_bit)}
    ].
