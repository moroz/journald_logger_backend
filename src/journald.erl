-module(journald).
-export([send/1]).

-spec(send(list()) -> ok).
send([]) ->
    ok;

send(List) when is_list(List) ->
    case gen_udp:open(0, [local]) of
        {error, SocketError} ->
            {error, SocketError};
        {ok, Socket} ->
            Serialized = [[K, serialize_value(V)] || {K, V} <- List],
            Result = gen_udp:send(Socket, {local, socket_path()}, 0, Serialized),
            gen_udp:close(Socket),
            Result
    end.

serialize_value(Binary) when is_binary(Binary) ->
    ByteSize = byte_size(Binary),
    [$\n, <<ByteSize:64/little>>, Binary, $\n];
serialize_value(List) when is_list(List) ->
    serialize_value(iolist_to_binary(List));
serialize_value(Int) when is_integer(Int) ->
    serialize_value(integer_to_binary(Int));
serialize_value(_) ->
    [].

socket_path() -> "/run/systemd/journal/socket".
