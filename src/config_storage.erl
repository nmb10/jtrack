%%    @author Kazbek Biasti <nmb.ten@gmail.com>
-module(config_storage).
-behavior(gen_server).

% Public methods
-export([
    get/1,
    set/2,
    update/1,
    get_all/0
]).


%% gen_server callbacks
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([handle_cast/2]).

start_link() ->
    logger:notice("[config_storage] start_link(): \n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Config1 = #{config => #{file => "./logs/config_storage.log"}, level => debug},

    logger:add_handler(config_storage_file_handler, logger_disk_log_h, Config1),
    logger:notice("Init config storage."),

    ConfigFileName = get_config_filename(),

    ConfigMap = case filelib:is_regular(ConfigFileName) of
        true ->
            {ok, BinaryData} = file:read_file(ConfigFileName),
            jiffy:decode(BinaryData, [return_maps]);
        false ->
            Default = #{
                jira_domain => nil,
                jira_bearer => nil,
                hours_limit => 8,
                featured_issues => []
            },
            Dump = jiffy:encode(Default, [pretty, use_nil]),
            filelib:ensure_dir(ConfigFileName),
            file:write_file(ConfigFileName, Dump),
            Default
    end,

    State = ConfigMap,
    {ok, State}.


%
% public API
%

% Sync methods.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


update(ConfigMap) ->
    gen_server:call(?MODULE, {update, ConfigMap}).


get_all() ->
    gen_server:call(?MODULE, {get_all}).


handle_call({get, Key}, _From, State) ->
    % #{en => #{
    {reply, maps:get(Key, State), State};


handle_call({get_all}, _From, State) ->
    % #{en => #{
    {reply, State, State};


handle_call({update, ConfigMap}, _From, State) ->
    % #{en => #{
    NewState = maps:merge(State, ConfigMap),
    Dump = jiffy:encode(NewState, [pretty]),
    ConfigFileName = get_config_filename(),
    filelib:ensure_dir(ConfigFileName),
    file:write_file(ConfigFileName, Dump),
    {reply, NewState, NewState};


handle_call({set, Key, Value}, _From, State) ->
    % #{en => #{
    NewState = maps:put(Key, Value, State),
    Dump = jiffy:encode(NewState, [pretty]),
    ConfigFileName = get_config_filename(),
    filelib:ensure_dir(ConfigFileName),
    file:write_file(ConfigFileName, Dump),
    {reply, Value, NewState}.


handle_info(track, _State) ->
    logger:notice("[config_storage] no info for now.: \n").


handle_cast(test, _State) ->
    logger:notice("[config_storage] no cast for now.: \n").


get_config_filename() ->
    FileName = <<".config.json">>,

    {ok, [[HomeDir]]} = init:get_argument(home),
    TrackerDir = filename:join([HomeDir, <<"jtrack/">>]),
    TrackFileName = filename:join([TrackerDir, FileName]),
    TrackFileName.
