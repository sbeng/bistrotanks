-module(bistrotanks).
-include("records.hrl").

%% API
-export([start/0, player_names/0, join_game/2, leave_game/1, add_spectator/2]).

%% @doc Start the application. Mainly useful for using `-s bistrotanks' as a command
%% line switch to the VM to make bistrotanks start on boot.
start() -> start(bistrotanks).

start(App) ->
  start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
  ok = start(Dep),
  start(App);
start_ok(App, {error, Reason}) ->
  erlang:error({app_start_failed, App, Reason}).

player_names() ->
  gen_server:call(bistrotanks_server, get_player_names).

join_game(UserPid, UserName) ->
  gen_server:cast(bistrotanks_server, {join_game, UserPid, UserName}).

leave_game(UserPid) ->
  gen_server:cast(bistrotanks_server, {leave_game, UserPid}).

add_spectator(UserPid, UserIp) ->
  gen_server:cast(message_broker, {add_listener, UserPid}),
  gen_server:call(bistrotanks_server, {register_new_user, UserPid, UserIp}).
