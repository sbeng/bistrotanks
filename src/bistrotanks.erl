-module(bistrotanks).

%% API
-export([start/0]).

%% @doc Start the application. Mainly useful for using `-s bistrotanks' as a command
%% line switch to the VM to make bistrotanks start on boot.
start() -> start(bistrotanks, temporary).

start(App) ->
  start_ok(App, application:start(App, permanent)).
start(App, temporary) ->
  start_ok(App, application:start(App, temporary)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
  ok = start(Dep),
  start(App);
start_ok(App, {error, Reason}) ->
  erlang:error({app_start_failed, App, Reason}).
