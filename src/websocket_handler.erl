-module(websocket_handler).
-behaviour(cowboy_websocket_handler).
-include("records.hrl").

%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  self() ! register_spectator,
  {ok, Req, #connection{}}.

%% simple ping/pong
websocket_handle({text, <<"ping">>}, Req, State) ->
  {reply, {text, <<"pong">>}, Req, State};
%% commands from user
%% spectators can join game only
websocket_handle({binary, EncodedMessage}, Req, State) when State#connection.connection_status =:= spectator ->
  {ok, Message} = msgpack:unpack(EncodedMessage),
  {ok, <<"join_game">>} = maps:find(<<"type">>, Message),
  {ok, Name} = maps:find(<<"name">>, Message),
  Reply = case re:run(binary_to_list(Name), "^([a-zA-Z0-9]{3,6})$") of
            {match, _} ->
              {ok, BusyNames} = bistrotanks_server:player_names(),
              AlreadyTaken = lists:member(Name, BusyNames),
              if true =:= AlreadyTaken ->
                ReplyMsg = msgpack:pack(message_broker:name_already_taken()),
                {reply, {binary, ReplyMsg}, Req, State};
                true ->
                  bistrotanks_server:join_game(self(), Name),
                  erlang:send_after(?PLAYER_ACTIONS_INTERVAL, self(), process_player_actions),
                  erlang:send_after(?PLAYER_NEWBORN_TIME, whereis(bistrotanks_server), {tank_grew_up, self()}),
                  {ok, Req, State#connection{connection_status = in_game, last_action_at = erlang:monotonic_time(seconds), last_fire_at = erlang:monotonic_time(seconds)}}
              end;
            nomatch ->
              ReplyMsg = msgpack:pack(message_broker:name_invalid()),
              {reply, {binary, ReplyMsg}, Req, State}
          end,
  Reply;
%% ignore all other messages by spectator
websocket_handle(Msg, Req, State) when State#connection.connection_status =:= spectator ->
  lager:info("Received unknown message from spectator: ~p", [Msg]),
  {ok, Req, State};

%% messages by player
websocket_handle({text, <<"leave_game">>}, Req, State) ->
  bistrotanks_server:leave_game(self()),
  {ok, Req, State#connection{connection_status = spectator}};

websocket_handle({text, <<"moving_up">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = add_move("up", State#connection.player_actions)}};
websocket_handle({text, <<"stop_moving_up">>}, Req, State) ->
  PlayerActions = lists:delete("up", State#connection.player_actions),
  {ok, Req, State#connection{player_actions = PlayerActions}};
websocket_handle({text, <<"moving_down">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = add_move("down", State#connection.player_actions)}};
websocket_handle({text, <<"stop_moving_down">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = lists:delete("down", State#connection.player_actions)}};
websocket_handle({text, <<"rotating_left">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = add_move("left", State#connection.player_actions)}};
websocket_handle({text, <<"stop_rotating_left">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = lists:delete("left", State#connection.player_actions)}};
websocket_handle({text, <<"rotating_right">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = add_move("right", State#connection.player_actions)}};
websocket_handle({text, <<"stop_rotating_right">>}, Req, State) ->
  {ok, Req, State#connection{player_actions = lists:delete("right", State#connection.player_actions)}};
websocket_handle({text, <<"fire">>}, Req, State) ->
  LastFire = -(State#connection.last_fire_at - erlang:monotonic_time(seconds)),
  UpdatedState = if LastFire > ?TANK_FIRE_RATE ->
                   PlayerActions = add_move("fire", State#connection.player_actions),
                   State#connection{player_actions = PlayerActions, last_fire_at = erlang:monotonic_time(seconds)};
                   true -> State
                 end,
  {ok, Req, UpdatedState};
%% ignore all other messages by player
websocket_handle(Msg, Req, State) ->
  lager:info("Received unknown message from player: ~p", [Msg]),
  {ok, Req, State}.

%% replies from server
websocket_info({'DOWN', MonitorRef, process, _ServerPid, _Cause}, Req, State) ->
  erlang:demonitor(MonitorRef),
  {shutdown, Req, State};
websocket_info(register_spectator, Req, State) ->
  erlang:monitor(process, bistrotanks_server),
  {ok, Message} = bistrotanks_server:add_spectator(self(), readable_user_ip(Req)),
  {reply, {binary, Message}, Req, State};
websocket_info({world_updates, Message}, Req, State) ->
  {reply, {binary, Message}, Req, State};
%% process moves of player
websocket_info(process_player_actions, Req, State) when State#connection.connection_status =:= in_game ->
  UpdatedState = if length(State#connection.player_actions) > 0 -> %% user wants to make some action
                     bistrotanks_server:process_player_actions(self(), State#connection.player_actions),
                     %% delete 'fire' action, it should be processed only once
                     UpdatedPlayerActions = lists:delete("fire", State#connection.player_actions),
                     State#connection{last_action_at = erlang:monotonic_time(seconds), is_moving = true, player_actions = UpdatedPlayerActions};
                   true ->
                     if State#connection.is_moving =:= true ->
                       bistrotanks_server:stop_player_actions_processing(self());
                       true -> ok
                     end,
                     %% check if player is active, otherwise move him to spectators
%%                      State#connection{is_moving = false, player_actions = []}
                     LastAction = -(State#connection.last_action_at - erlang:monotonic_time(seconds)),
                     if LastAction > ?PLAYER_ACTIONS_TIMEOUT ->
                       bistrotanks_server:leave_game(self()),
                       State#connection{connection_status = spectator, is_moving = false, player_actions = []};
                       true -> State#connection{is_moving = false, player_actions = []}
                     end
                 end,
  erlang:send_after(?PLAYER_ACTIONS_INTERVAL, self(), process_player_actions),
  {ok, Req, UpdatedState};
%% ignore timer event if user left the game
websocket_info(process_player_actions, Req, State) ->
  {ok, Req, State};
websocket_info(schedule_respawn, Req, State) ->
  erlang:send_after(?RESPAWN_AFTER, self(), respawn),
  {ok, Req, State#connection{is_moving = false}};
websocket_info(respawn, Req, State) ->
  {ok, Req, State#connection{is_moving = true}};

websocket_info(Info, Req, State) ->
  lager:info("Received from erlang system: ~p", [Info]),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

%% Private
readable_user_ip(Request) ->
  {{IP, _}, _}  = cowboy_req:peer(Request),
  inet:ntoa(IP).

add_move(Move, PlayerActions) ->
  AlreadyPresent = lists:any(fun(E) -> E == Move end, PlayerActions),
  if false =:= AlreadyPresent -> [Move|PlayerActions];
    true -> PlayerActions
  end.
