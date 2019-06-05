-module(journald_logger_formatter).

-export([format/4]).

format(Level, Msg, Timestamp, Metadata) ->
    [
     format_timestamp(Timestamp),
     " [",
     atom_to_binary(Level, utf8),
     "] ",
     format_metadata(Metadata),
     Msg,
     10
    ].

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

format_metadata(_) ->
    [].

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
