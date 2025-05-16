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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
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
    gen_server:call(?MODULE, {get_issues}).


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

    io:format("[issue_storage] Fetching issues now...\n"),

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
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(
        Method, URL, Headers, Payload, Options),

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

    MyIssues = case StatusCode of
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
            io:format("[issue_storage] Failed to fetch issues. Returning issues from previous success response (if exists). StatusCode: ~p\n", [StatusCode]),

            FileName = '/tmp/jtrack-issues-cache.json',
            MyIssues0 = case filelib:is_regular(FileName) of
                true ->
                    {ok, BinaryData} = file:read_file(FileName),
                    jiffy:decode(BinaryData, [return_maps]);
                false ->
                    []
            end,
            MyIssues0
    end,
    FeaturedIssues = config_storage:get(<<"featured_issues">>),
    io:format("[issue_storage] Issues fetching finished.\n"),
    erlang:send_after(1 * 60 * 1000, self(), fetch_issues),
    NewState = {FeaturedIssues, MyIssues, CurrentIssue},
    {noreply, NewState}.
