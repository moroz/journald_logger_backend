-module(journald_logger_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, terminate/2]).

-define(DEFAULTS, [
                   {level, info},
                   {formatter, journald_logger_formatter}
                  ]).

init({?MODULE, Name}) ->
    {ok, configure(Name)}.

terminate(_Reason, _State) ->
    ok.

handle_call(_, State) ->
    {ok, State}.

handle_event({Level, _, {_, Msg, Ts, Md}}, State) ->
    log_event(Level, Msg, Ts, Md, State);

handle_event(flush, State) ->
    {ok, State};

handle_event(_, State) ->
    io:format("received event~n"),
    {ok, State}.

configure(Name) ->
    {ok, Config} = application:get_env(?MODULE, Name),
    State = parse_config(Config),
    io:format("~p~n", [State]),
    State.

parse_config(Config) when is_map(Config) ->
    [Level, Formatter] =
        [maps:get(K, Config, Def) || {K, Def} <- ?DEFAULTS],
    #{formatter => Formatter, level => Level};
parse_config(Config) when is_list(Config) ->
    [Level, Formatter] =
        [proplists:get_value(K, Config, Def) || {K, Def} <- ?DEFAULTS],
    #{formatter => Formatter, level => Level}.

log_event(Level, Msg, Ts, Md, State = #{formatter := F}) ->
    Output = F:format(Level, Msg, Ts, Md),
    io:format(Output),
    {ok, State}.
