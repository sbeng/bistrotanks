-module(bistrotanks_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
    {main_server, {bistrotanks_server, start_link, []}, transient, 5000, worker, [bistrotanks_server]},
    {messages, {message_broker, start_link, []}, transient, 5000, worker, [message_broker]}
  ],
	{ok, {{one_for_all, 1, 5}, Procs}}.
