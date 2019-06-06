-module(logger_journald_helper).

-export([level_to_num/1, format_timestamp/1, to_list/1]).

level_to_num(debug) -> 7;
level_to_num(info) -> 6;
level_to_num(notice) -> 5;
level_to_num(warning) -> 4;
level_to_num(warn) -> 4;
level_to_num(error) -> 3;
level_to_num(critical) -> 2;
level_to_num(alert) -> 1;
level_to_num(emergency) -> 0.


format_timestamp({Date, Time}) ->
    [
     format_date(Date),
     32,
     format_time(Time)
    ].

format_date({Y,M,D}) ->
    [integer_to_binary(Y), 45, pad2(M), 45, pad2(D)].

format_time({H,M,S,Ms}) ->
    [pad2(H), 58, pad2(M), 58, pad2(S), 46, pad3(Ms)].

pad2(Int) when Int < 10 ->
    [48, integer_to_binary(Int)];
pad2(Int) ->
    integer_to_binary(Int).

pad3(Int) when Int < 10 ->
    [48, 48, integer_to_binary(Int)];
pad3(Int) when Int < 100 ->
    [48, integer_to_binary(Int)];
pad3(Int) ->
    integer_to_binary(Int).

% Convert any Erlang term to list format that can be
% passed to journald as value
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_float(V)   -> float_to_list(V);
to_list(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_list(V) when is_function(V)-> io_lib:format("~p", [V]);
to_list(V) when is_port(V)    -> io_lib:format("~p", [V]);
to_list(V) when is_pid(V)     -> pid_to_list(V);
to_list(V) when is_tuple(V)   -> io_lib:format("~p", [V]);
to_list(V) when is_map(V)     -> io_lib:format("~p", [V]);
to_list(V) when is_list(V) ->
    try io_lib:format('~s', [V])
    catch _:_ ->
            io_lib:format('~p', [V])
    end;
to_list(V) -> V.
