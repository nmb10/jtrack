-module(work_session_storage_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("Starting share_storage supervisor.\n"),
    Procs = [
        #{
            id => work_session_storage,
            start => {work_session_storage, start_link, []},
            % restart => permanent,
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => [work_session_storage]
        }
    ],
    % Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
