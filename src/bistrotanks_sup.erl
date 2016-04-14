-module(bistrotanks_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
    {main_server, {bistrotanks_server, start_link, []}, permanent, 5000, worker, [bistrotanks_server]}
  ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
