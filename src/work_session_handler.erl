-module(work_session_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_work_sessions/2]).
-export([create_or_update_work_session/2]).
-export([allowed_methods/2]).


init(Req, Opts) ->
    Config1 = #{config => #{file => "../../logs/work_session_handler.log"}, level => debug},
    logger:add_handler(work_session_handler_file_handler, logger_disk_log_h, Config1),
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
      {{<<"application">>, <<"json">>, []}, create_or_update_work_session}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_work_sessions}
    ], Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>], Req, State}.


create_or_update_work_session(Req, State) ->
    Op = maps:get(op, State),

    {Body, Req1, State1} = case Op of
        create ->
            % create_session(Req, State);
            io:format("Creating work session FIXME: DropUpdating work session!!!~p\n", [sessionId]),
            {nil, nil, nil};
        update ->
            update_work_session(Req, State)
    end,
    {Body, Req1, State1}.


update_work_session(Req, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req),

    Session = jiffy:decode(Data, [return_maps]),
    UpdatedSession = Session,
    % tracker:toggle_session(Session),
    work_session_storage:save_work_session(Session),
    Req1 = cowboy_req:set_resp_body(jiffy:encode(UpdatedSession), Req),
    cowboy_req:reply(201, Req1),
    {stop, Req1, State}.


get_work_sessions(Req, State) ->
    % FIXME: Implement.
    Response = jiffy:encode(
        #{},
        [use_nil]),
    {Response, Req, State}.
