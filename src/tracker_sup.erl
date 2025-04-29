-module(tracker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("Starting share_storage supervisor.\n"),
    Procs = [
        #{
            id => tracker,
            start => {tracker, start_link, []},
            % restart => permanent,
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => [tracker]
        }
    ],
    % Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
