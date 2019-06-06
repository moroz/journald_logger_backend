-module(logger_journald_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, terminate/2]).

-define(DEFAULTS, [
                   {level, info},
                   {formatter, logger_journald_formatter}
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
    Config = application:get_env(?MODULE, Name, #{}),
    parse_config(Config).

parse_config(Config) when is_map(Config) ->
    [Level, Formatter] =
        [maps:get(K, Config, Def) || {K, Def} <- ?DEFAULTS],
    #{formatter => Formatter, level => Level};
parse_config(Config) when is_list(Config) ->
    [Level, Formatter] =
        [proplists:get_value(K, Config, Def) || {K, Def} <- ?DEFAULTS],
    #{formatter => Formatter, level => Level}.


log_event(Level, Msg, Ts, Md, State = #{formatter := F}) ->
    Text = F:format(Level, Msg, Ts, Md),
    Metalist = [{"MESSAGE", Text},
                {"PRIORITY", logger_journald_helper:level_to_num(Level)}],
    journald_api:sendv(Metalist),
    {ok, State}.
