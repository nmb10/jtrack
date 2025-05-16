-module(notes_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_notes/2]).
-export([create_or_update_notes/2]).
-export([allowed_methods/2]).


init(Req, Opts) ->
    Config1 = #{config => #{file => "../../logs/notes_handler.log"}, level => debug},
    logger:add_handler(notes_handler_file_handler, logger_disk_log_h, Config1),
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
      {{<<"application">>, <<"json">>, []}, create_or_update_notes}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_notes}
    ], Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>], Req, State}.


create_or_update_notes(Req, State) ->
    Op = maps:get(op, State),

    {Body, Req1, State1} = case Op of
        create ->
            % create_session(Req, State);
            % io:format("Creating notes!!!~p\n", [sessionId]),
            {nil, nil, nil};
        update ->
            update_notes(Req, State)
    end,
    {Body, Req1, State1}.


update_notes(Req, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req),

    PostData = jiffy:decode(Data, [return_maps]),
    Content = maps:get(<<"content">>, PostData),
    % tracker:toggle_session(Session),
    NotesFileName = get_notes_filename(),
    filelib:ensure_dir(NotesFileName),
    file:write_file(NotesFileName, Content),

    Req1 = cowboy_req:set_resp_body(jiffy:encode(PostData), Req),
    cowboy_req:reply(201, Req1),
    {stop, Req1, State}.


get_notes(Req, State) ->
    NotesFileName = get_notes_filename(),

    NotesData = case filelib:is_regular(NotesFileName) of
        true ->
            {ok, BinaryData} = file:read_file(NotesFileName),
            BinaryData;
        false ->
            <<"">>
    end,

    Response = jiffy:encode(
        #{<<"content">> => NotesData},
        [use_nil]),
    {Response, Req, State}.


get_notes_filename() ->
    FileName = <<"notes.md">>,

    {ok, [[HomeDir]]} = init:get_argument(home),
    TrackerDir = filename:join([HomeDir, <<"jtrack/">>]),
    NotesFileName = filename:join([TrackerDir, FileName]),
    NotesFileName.
