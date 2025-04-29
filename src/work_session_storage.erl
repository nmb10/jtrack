%%    @author Kazbek Biasti <nmb.ten@gmail.com>
-module(work_session_storage).
-behavior(gen_server).

% Public methods
-export([
    % Sync.
    % create_custom_set/1,
    save_work_session/1
    % update_custom_set/4
]).


%% gen_server callbacks
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
% -export([handle_info/2]).
-export([handle_cast/2]).


start_link() ->
    % logger:notice("[custom_set_storage] start_link(): \n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    State = #{},
    {ok, State}.


%
% public API
%

% Sync methods.

save_work_session(Session) ->
    gen_server:cast(?MODULE, {save_work_session, Session}).

% Async method.
%start_train_game(Players, Language, Topic, Level, Method) ->
%    gen_server:cast(?MODULE, {start, <<"train">>, Players, Language, Topic, Level, Method}),
%    ok.


handle_call({fixme}, _From, State) ->
    {reply, #{}, State}.


handle_cast({save_work_session, Session}, State) ->
    IssueKey = maps:get(<<"issue_key">>, Session),
    StartedAt = maps:get(<<"started_at">>, Session),
    IssueMinutes = maps:get(<<"minutes">>, Session),
    Method = post,
    Url = io_lib:format(
            "https://~s/rest/api/2/issue/~s/worklog",
            [config_storage:get(<<"jira_base_url">>), IssueKey]),
    ReqBody = jiffy:encode(#{
        <<"started">> => <<StartedAt/binary, ".000+0000">>,
        % <<"started">>: "2025-02-28T10:30:00.000+0000",
        <<"timeSpentSeconds">> => IssueMinutes * 60
    }),

    Headers = [{"Authorization", "Bearer " ++ config_storage:get(<<"jira_bearer">>)},
               {"Accept", "application/json"},
               {"Content-Type", "application/json"}],

    Options = [],
    {ok, ClientRef} = hackney:request(Method, Url, Headers, stream, Options),
    ok = hackney:send_body(ClientRef, ReqBody),
    {ok, StatusCode, _Headers, ClientRef} = hackney:start_response(ClientRef),
    {ok, _Body} = hackney:body(ClientRef),

    case StatusCode of
        201 ->
            io:format(
                "Time saved. Mark session as logged to jira. code: ~p, session: ~p\n",
                [StatusCode, Session]),
            % When session saved we need to write it to day dump.
            tracker:mark_as_logged(Session);
        _ ->
            io:format("Failed to save time. StatusCode: ~p\n", [StatusCode]),
            []
    end,

    {noreply, State}.
