-module(issue_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_issues/2]).
-export([create_or_update_issue/2]).
-export([allowed_methods/2]).


init(Req, Opts) ->
    Config1 = #{config => #{file => "../../logs/issue_handler.log"}, level => debug},
    logger:add_handler(issue_handler_file_handler, logger_disk_log_h, Config1),
    % logger:notice("Init issue handler endpoint."),
    [Op | _] = Opts,
    % State = #state{op=Op},
    NewState = #{op => Op},
    {cowboy_rest, Req, NewState}.


content_types_accepted(Req, State) ->
    % {ok, Data, Req0} = cowboy_req:read_body(Req),
    % io:format("Data: ~p~n", [Data]),
    % io:format("Req: ~p~n", [Req]),
    {[
      {{<<"application">>, <<"json">>, []}, create_or_update_issue}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_issues}
    ], Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>], Req, State}.


% Creates issue for given term.
create_or_update_issue(Req, State) ->
    Op = maps:get(op, State),

    {Body, Req1, State1} = case Op of
        create ->
            % create_issue(Req, State);
            io:format("Creating issue FIXME: DropUpdating issue!!!~p\n", [issueId]),
            {nil, nil, nil};
        update ->
            update_issue(Req, State)
    end,
    {Body, Req1, State1}.


update_issue(Req, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req),
    Issue = jiffy:decode(Data, [return_maps]),
    UpdatedIssue = Issue,
    tracker:toggle_issue(Issue),
    Req1 = cowboy_req:set_resp_body(jiffy:encode(UpdatedIssue), Req),
    cowboy_req:reply(201, Req1),
    {stop, Req1, State}.


get_issues(Req, State) ->
    {FeaturedIssues, Issues} = issue_storage:get_issues(),
    CurrentIssue = tracker:get_current_issue(),

    Stat = tracker:get_today_stat(),
    JtrackWorklog = tracker:get_worklog_history(),
    Response = jiffy:encode(
        #{<<"current_issue">> => CurrentIssue,
          <<"featured_issues">> => FeaturedIssues,
          <<"issues">> => Issues,
          <<"stat">> => Stat,
          <<"jtrack_worklog">> => JtrackWorklog},
        [use_nil]),
    {Response, Req, State}.
