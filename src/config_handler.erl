-module(config_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_config/2]).
-export([update_config/2]).
-export([allowed_methods/2]).


init(Req, Opts) ->
    Config1 = #{config => #{file => "../../logs/config_handler.log"}, level => debug},
    logger:add_handler(config_handler_file_handler, logger_disk_log_h, Config1),
    % logger:notice("Init session handler endpoint."),
    [Op | _] = Opts,
    % State = #state{op=Op},
    NewState = #{op => Op},
    {cowboy_rest, Req, NewState}.


content_types_accepted(Req, State) ->
    % {ok, Data, Req0} = cowboy_req:read_body(Req),
    % io:format("Data: ~p~n", [Data]),
    % io:format("Req: ~p~n", [Req]),
    {[
      {{<<"application">>, <<"json">>, []}, update_config}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_config}
    ], Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>], Req, State}.


update_config(Req, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req),
    PartialConfig = jiffy:decode(Data, [return_maps]),
    config_storage:update(PartialConfig),
    Req1 = cowboy_req:set_resp_body(jiffy:encode(PartialConfig), Req),
    cowboy_req:reply(201, Req1),
    {stop, Req1, State}.


get_config(Req, State) ->
    ConfigMap = config_storage:get_all(),
    JiraBearer = maps:get(<<"jira_bearer">>, ConfigMap),
    JiraBearer1 = case JiraBearer == nil of
        true -> JiraBearer;
        _ ->
            Part1 = binary:part(JiraBearer, 1, 2),
            <<Part1/binary, "***">>
    end,
    ConfigMap1 = maps:update(<<"jira_bearer">>, JiraBearer1, ConfigMap),
    Response = jiffy:encode(ConfigMap1, [use_nil]),
    {Response, Req, State}.
