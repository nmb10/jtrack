-module(jtrack_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [  % {"/", index_handler, []},
               {"/api/v1/issues", issue_handler, [update]},
               {"/api/v1/sessions", work_session_handler, [update]},
               {"/", cowboy_static, {priv_file, jtrack, "static/index.html"}},
               {"/static/app.js", cowboy_static, {priv_file, jtrack, "static/app.js"}}
              ]
        }
        % {'/', cowboy_static, {priv_file, jtrack_app, 'src/static/index.html'}}

    ]),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8087}],
        #{env => #{dispatch => Dispatch}}
    ),
    %ServiceConfig = [],
    %inets:start(httpc, ServiceConfig),
    work_session_storage_sup:start_link(),
    issue_storage_sup:start_link(),
    tracker_sup:start_link(),
    config_storage_sup:start_link(),
    jtrack_sup:start_link().

stop(_State) ->
    ok.
