%%    @author Kazbek Biasti <nmb.ten@gmail.com>
-module(tracker).
-behavior(gen_server).

% Public methods
-export([
    % Sync.
    toggle_issue/1,
    get_current_issue/0,
    get_worklog_history/0,
    get_today_stat/0,
    mark_as_logged/1
]).


%% gen_server callbacks
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([handle_cast/2]).


-define(INTERVAL, 1000).  % One second.

start_link() ->
    logger:notice("[tracker] start_link(): \n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    Config1 = #{config => #{file => "./logs/tracker.log"}, level => debug},

    logger:add_handler(tracker_file_handler, logger_disk_log_h, Config1),
    logger:notice("Init tracker."),

    CurrentIssue = nil,

    NowDate = get_today_iso(),
    TrackFileName = get_track_filename(NowDate),

    TrackData = case filelib:is_regular(TrackFileName) of
        true ->
            {ok, BinaryData} = file:read_file(TrackFileName),
            jiffy:decode(BinaryData, [return_maps]);
        false ->
            []
    end,

    State = {
      CurrentIssue,
      {NowDate, TrackData}
    },
    {ok, State}.


%
% public API
%

% Sync methods.

toggle_issue(Issue) ->
    gen_server:call(?MODULE, {toggle, Issue}).


get_worklog_history() ->
    gen_server:call(?MODULE, {get_worklog_history}).


get_current_issue() ->
    gen_server:call(?MODULE, {get_current_issue}).


get_today_stat() ->
    gen_server:call(?MODULE, {get_today_stat}).


mark_as_logged(Session) ->
    gen_server:cast(?MODULE, {mark_as_logged, Session}).


handle_call({get_today_stat}, _From, State) ->
    % #{en => #{
    {CurrentIssue, {TrackDate, TrackData}} = State,

    % Example of TrackData
    % [
    %   [<<"JT-10575">>,[<<"2025-02-24T16:13:07">>,8]],
    %   [<<"JT-10571">>,[<<"2025-02-24T16:17:11">>,5]]
    % ]

    NowDate = get_today_iso(),

    ActualTrackData = case TrackDate == NowDate of
        true -> TrackData;
        _ -> []
    end,
    Stat = get_stat(ActualTrackData),
    NewState = {CurrentIssue, {NowDate, ActualTrackData}},
    {reply, Stat, NewState};


handle_call({get_worklog_history}, _From, State) ->

    % Find dates to cover this and previous weeks days.
    % {EndDate, _} = calendar:now_to_datetime(erlang:timestamp()),
    {EndDate, _} = erlang:localtime(),
    StartDate = substract(EndDate, 14),
    PeriodDays = lists:reverse([calendar:gregorian_days_to_date(X) || X <- lists:seq(calendar:date_to_gregorian_days(StartDate), calendar:date_to_gregorian_days(EndDate))]),

    % Load tracker logs.
    % Debug for one record:
    % History = date_worklog(lists:nth(1, PeriodDays)),
    History = lists:map(fun date_worklog/1, PeriodDays),
    {reply, History, State};


handle_call({get_current_issue}, _From, State) ->
    % #{en => #{
    {CurrentIssue, {_TrackDate, _TrackData}} = State,
    {reply, CurrentIssue, State};


handle_call({toggle, Issue}, _From, State) ->
    {CurrentIssue, {_TrackDate, TrackData}} = State,
    NewCurrentIssue = case CurrentIssue == nil of
        true ->
            Issue;
        _ ->
          case maps:get(<<"key">>, CurrentIssue) == maps:get(<<"key">>, Issue) of
                true ->
                    % The same issue toggled. Means stop.
                    nil;
                _ ->
                    Issue
          end
    end,

    NewState = {NewCurrentIssue, {_TrackDate, TrackData}},
    case {CurrentIssue, NewCurrentIssue} of
        {nil, nil} ->
            logger:notice("Nothing to track. Will not trigger track.\n");
        {nil, _} ->
            logger:notice(
                "[tracker, ~s]. New issue tracking started.\n",
                [maps:get(<<"key">>, NewCurrentIssue)]),
            erlang:send_after(500, self(), track);
        {_, nil} ->
            logger:notice(
                "[tracker, ~s] Issue turned off. Call tracker to stop it.\n",
                [maps:get(<<"key">>, CurrentIssue)]),
            erlang:send_after(500, self(), track);
        _ ->
            logger:notice("Issues switched. Do nothing.\n")
    end,
    {reply, NewCurrentIssue, NewState}.


handle_cast({mark_as_logged, Session}, State) ->
    StartedAt = maps:get(<<"started_at">>, Session),
    WorkSessionId = maps:get(<<"session_id">>, Session),

    {match, [[Date]]} = re:run(StartedAt, "[0-9]+-[0-9]+-[0-9]+", [global, {capture, all, binary}]),
    FileName = <<"track-", Date/binary, ".json">>,
    {ok, [[HomeDir]]} = init:get_argument(home),
    TrackerDir = filename:join([HomeDir, <<"jtrack/tracker/raw">>]),
    TrackFileName = filename:join([TrackerDir, FileName]),

    TrackData = case filelib:is_regular(TrackFileName) of
        true ->
            {ok, BinaryData} = file:read_file(TrackFileName),
            jiffy:decode(BinaryData, [return_maps]);
        false ->
            []
    end,

    UpdateSyncStatus = fun([IssueKey0, [WorkSessionId0, StartedAt0, MinutesSpent0, SyncStatus]]) ->
        NewStatus = case WorkSessionId == WorkSessionId0 of
            true -> true;
            _ -> SyncStatus
        end,
        [IssueKey0, [WorkSessionId0, StartedAt0, MinutesSpent0, NewStatus]]
    end,
    UpdatedTrackData = lists:map(UpdateSyncStatus, TrackData),
    Dump = jiffy:encode(UpdatedTrackData, [pretty]),
    filelib:ensure_dir(TrackFileName),
    file:write_file(TrackFileName, Dump),
    {noreply, State}.


handle_info(track, State) ->
    {CurrentIssue, {TrackDate, TrackData}} = State,

    case CurrentIssue of
        nil ->
            logger:notice("Nothing to track. Tracking and dumping stopped...\n"),
            {noreply, State};
        _ ->
            Stat = get_stat(TrackData),
            % Do not allow to work more than HOURS_LIMIT hours
            case lists:sum(maps:values(Stat)) >= config_storage:get(<<"hours_limit">>) * 60 of
                true ->
                    logger:notice(
                        "[tracker, ~s] timer can not track more than 8 hours per day. Stopping...\n",
                        [maps:get(<<"key">>, CurrentIssue)]),
                    erlang:send_after(500, self(), track),
                    NewState = {nil, {TrackDate, TrackData}},
                    {noreply, NewState};
                _ ->
                    logger:notice(
                        "[tracker, ~s] Adding minutes and dump...\n",
                        [maps:get(<<"key">>, CurrentIssue)]),

                    NowDate = get_today_iso(),
                    NewTrackData = update_track_data(CurrentIssue, TrackDate == NowDate, TrackData),
                    Dump = jiffy:encode(NewTrackData, [pretty]),
                    TrackFileName = get_track_filename(NowDate),
                    filelib:ensure_dir(TrackFileName),
                    file:write_file(TrackFileName, Dump),
                    logger:notice(
                        "[tracker, ~s] 1 minute added and data dumped\n",
                        [maps:get(<<"key">>, CurrentIssue)]),
                    erlang:send_after(60 * 1000, self(), track),
                    NewState = {CurrentIssue, {NowDate, NewTrackData}},
                    {noreply, NewState}
            end
    end.


% datetime example
get_now_iso() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),  % calendar:now_to_datetime(erlang:timestamp()),

    Ret = list_to_binary(
        io_lib:format(
            "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w",
            [Year, Month, Day, Hour, Min, Sec])),
    Ret.


% Returns today date in ISO format.
get_today_iso() ->
    % {Today, _} = calendar:now_to_datetime(erlang:timestamp()),
    {Today, _} = erlang:localtime(),
    date_to_iso(Today).


date_to_iso(Date) ->
    {Year, Month, Day} = Date,
    list_to_binary(
        io_lib:format(
            "~.4.0w-~.2.0w-~.2.0w",
            [Year, Month, Day])).


get_track_filename(Date) ->
    FileName = <<"track-", Date/binary, ".json">>,

    {ok, [[HomeDir]]} = init:get_argument(home),
    TrackerDir = filename:join([HomeDir, <<"jtrack/tracker/raw">>]),
    TrackFileName = filename:join([TrackerDir, FileName]),
    TrackFileName.


% substract N days from given Date. Returns new date.
substract(Date, N) ->
    New = calendar:date_to_gregorian_days(Date) - N,
    calendar:gregorian_days_to_date(New).


date_worklog(Date) ->
    DateIso = date_to_iso(Date),
    TrackFileName = get_track_filename(DateIso),

    TrackData = case filelib:is_regular(TrackFileName) of
        true ->
            {ok, BinaryData} = file:read_file(TrackFileName),
            jiffy:decode(BinaryData, [return_maps]);
        false ->
            []
    end,
    [DateIso, TrackData].


update_track_data(CurrentIssue, _, []) ->
    % New issue, no TrackData (issue just toggled)
    WorkSessionId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    [[maps:get(<<"key">>, CurrentIssue), [WorkSessionId, get_now_iso(), 1, false]]];


update_track_data(CurrentIssue, true, TrackData) ->
    % If the same issue, add one minute to issue task.
    [RecentIssueTrack | Rest] = TrackData,
    [RecentIssueKey, [WorkSessionId, SessionStartTime, RecentIssueTime, _isSaved]] = RecentIssueTrack,
    CurrentIssueKey = maps:get(<<"key">>, CurrentIssue),
    case RecentIssueKey == CurrentIssueKey of
      true ->
        % Replace recent (first) issue in track data.
        UpdatedIssueTrack = [RecentIssueKey, [WorkSessionId, SessionStartTime, RecentIssueTime + 1, false]],
        [UpdatedIssueTrack] ++ Rest;
      _ ->
        % Add new record to track data.
        NewWorkSessionId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
        [[CurrentIssueKey, [NewWorkSessionId, get_now_iso(), 1, false]]] ++ TrackData
    end;


update_track_data(CurrentIssue, _, _) ->
    WorkSessionId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    [[maps:get(<<"key">>, CurrentIssue), [WorkSessionId, get_now_iso(), 1, false]]].


get_stat(TrackData) ->
    % Example of TrackData
    % [
    %   [<<"JT-10575">>,[<<"1123">>, <<"2025-02-24T16:13:07">>,8]],
    %   [<<"JT-10571">>,[<<"1342">>, <<"2025-02-24T16:17:11">>,5]]
    % ]
    Stat = lists:foldl(
       fun([Key, [_, _, Count, _]], Acc) -> maps:put(Key, maps:get(Key, Acc, 0) + Count, Acc) end,
       #{},
       TrackData),
    Stat.
