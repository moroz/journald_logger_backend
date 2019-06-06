-module(logger_journald_formatter).

-export([format/4]).

format(Level, Msg, _Timestamp, Metadata) ->
    [
     91, % ?[
     atom_to_binary(Level, utf8),
     "] ",
     format_metadata(Metadata),
     Msg
    ].

format_metadata(Metadata) ->
    io_lib:format("~p", [Metadata]).
