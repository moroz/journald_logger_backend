-module(logger_journald_formatter).

-export([format/4, format_metadata/1]).

-define(PREFIXED_METADATA, [pid,file,line,module,function,node,date,time,message,severity,application]).

format(Level, Msg, _Timestamp, _Metadata) ->
    [
     91, % ?[
     atom_to_binary(Level, utf8),
     "] ",
     Msg
    ].

format_metadata(Metadata) ->
    Metalist = [{format_key(K), logger_journald_helper:to_list(V)}
      || {K, V} <- Metadata],
    io:format("~p~n", [Metalist])
.

format_key(K) ->
    List = normalize_key(K),
    Subbed = upper_and_sub(List),
    maybe_prefix_key(Subbed, should_prefix(K)).

normalize_key(K) when is_atom(K) ->
    atom_to_list(K);
normalize_key(K) when is_binary(K) ->
    binary_to_list(K);
normalize_key(K) when is_list(K) ->
    K.

upper_and_sub(K) ->
    re:replace(string:to_upper(K), "^[^a-zA-Z0-9]", "_", [global, {return, list}]).

maybe_prefix_key(K, true) ->
    "ERL_" ++ K;
maybe_prefix_key(K, _) ->
    K.

should_prefix(K) when is_atom(K) ->
    lists:any(fun(Elem) -> K == Elem end, ?PREFIXED_METADATA).
