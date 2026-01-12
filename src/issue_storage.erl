%%    @author Kazbek Biasti <nmb.ten@gmail.com>
-module(issue_storage).
-behavior(gen_server).

% Public methods
-export([
    % Sync.
    % create_custom_set/1,
    get_issues/0
    % update_custom_set/4
]).


%% gen_server callbacks
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([handle_cast/2]).


-define(INTERVAL, 1000).  % One second.

start_link() ->
    % logger:notice("[custom_set_storage] start_link(): \n"),
    logger:notice("[issue_storage] start_link(): \n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->

    Config1 = #{config => #{file => "./logs/issue_storage.log"}, level => debug},
    logger:add_handler(issue_storage_file_handler, logger_disk_log_h, Config1),
    logger:notice("Init issue storage."),

    erlang:send_after(?INTERVAL, self(), fetch_issues),

    Issues = [],
    FeaturedIssues = [],
    CurrentIssue = nil,
    {ok, {FeaturedIssues, Issues, CurrentIssue}}.


%
% public API
%

% Sync methods.
get_issues() ->
  try gen_server:call(?MODULE, {get_issues})
  catch exit:{timeout,_} ->
    % exit:{timeout,_} -> {ok, task_active}
    % Is running fetch_issues.
    logger:notice("[issue_storage] timeout error - fetch_issues is running now, returning from cache."),
    MyIssues = get_from_cache(),
    FeaturedIssues = config_storage:get(<<"featured_issues">>),
    {MyIssues, FeaturedIssues}
  end.


% Async method.
%start_train_game(Players, Language, Topic, Level, Method) ->
%    gen_server:cast(?MODULE, {start, <<"train">>, Players, Language, Topic, Level, Method}),
%    ok.


handle_call({get_issues}, _From, State) ->
    {FeaturedIssues, Issues, _} = State,
    {reply, {FeaturedIssues, Issues}, State}.


handle_cast({fixme, Issue}, State) ->
    {FeaturedIssues, Issues, CurrentIssue} = State,
    io:format("Issue...~p\n", [Issue]),
    {noreply, {FeaturedIssues, Issues, CurrentIssue}}.


handle_info(fetch_issues, State) ->
    {_, _, CurrentIssue} = State,

    logger:notice("[issue_storage] Fetching issues now..."),
    % For testing long fetches
    % timer:sleep(8000),

    JqlQuery = uri_string:quote(
        "assignee = currentUser() and status in (\"New\", \"To Do\", \"In Progress\")"),

    % hackney
    %
    Method = get,
    URL = io_lib:format(
            "https://~s/rest/api/2/search?jql=~s",
            [config_storage:get(<<"jira_domain">>), JqlQuery]),
    Headers = [{"Authorization", "Bearer " ++ config_storage:get(<<"jira_bearer">>)}],
    Payload = <<>>,
    Options = [],

    % Simplify issue structure
    JiraDomain = config_storage:get(<<"jira_domain">>),

    Fun = fun (Issue) ->
        Fields = maps:get(<<"fields">>, Issue),
        IssueKey = maps:get(<<"key">>, Issue),
        #{<<"title">> => maps:get(<<"summary">>, Fields),
          <<"key">> => IssueKey,
          <<"url">> => <<"https://", JiraDomain/binary, "/browse/", IssueKey/binary>>,
          <<"status">> => maps:get(<<"name">>, maps:get(<<"status">>, Fields))}
    end,
    % FIXME: Move long fetch to the process to not block the whole issue_storage
    %  Pid = spawn_link(?MODULE, fetch_issuessome_long_running_task, [State]),
    HackneyResponse = hackney:request(Method, URL, Headers, Payload, Options),

    MyIssues = case HackneyResponse of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            % Success
            MyIssues1 = case StatusCode of
                200 ->
                    {ok, ResponseBody} = hackney:body(ClientRef),
                    SearchResponseMap = jiffy:decode(ResponseBody, [return_maps]),
                    IssueList = maps:get(<<"issues">>, SearchResponseMap),

                    MyIssues0 = lists:map(Fun, IssueList),
                    Dump = jiffy:encode(MyIssues0, [pretty]),
                    FileName = '/tmp/jtrack-issues-cache.json',
                    filelib:ensure_dir(FileName),
                    file:write_file(FileName, Dump),
                    MyIssues0;
                _ ->
                    % FIXME: Notify frontend about the problem with issue upgrade.
                    logger:notice(
                        "[issue_storage] Failed to fetch issues. Returning issues from previous success response (if exists). StatusCode: ~p\n",
                        [StatusCode]),
                    get_from_cache()
            end,
            MyIssues1;
        {error, connect_timeout} ->
            %% Handle connection timeout specifically
            logger:error("Connection timed out.~n"),
            get_from_cache();
        {error, timeout} ->
            %% Handle receive (response body) timeout specifically
            logger:error("Receive timed out.~n"),
            get_from_cache();
        {error, Reason} ->
            %% Handle other general errors
            logger:error("Hackney request failed: ~p~n", [Reason]),
            get_from_cache()
    end,
    FeaturedIssues = config_storage:get(<<"featured_issues">>),
    logger:notice("[issue_storage] Issues fetching finished."),

    erlang:send_after(1 * 60 * 1000, self(), fetch_issues),
    NewState = {FeaturedIssues, MyIssues, CurrentIssue},
    {noreply, NewState}.


get_from_cache() ->
    FileName = '/tmp/jtrack-issues-cache.json',
    MyIssues0 = case filelib:is_regular(FileName) of
        true ->
            {ok, BinaryData} = file:read_file(FileName),
            jiffy:decode(BinaryData, [return_maps]);
        false ->
            []
    end,
    MyIssues0.
