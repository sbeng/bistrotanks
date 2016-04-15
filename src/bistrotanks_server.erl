-module(bistrotanks_server).
-behaviour(gen_server).
-include("records.hrl").

-export([process_player_actions/2,
         stop_player_actions_processing/1, kill_player/4, kill_bullet/2, update_bullet/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
process_player_actions(UserPid, Actions) ->
  gen_server:cast(bistrotanks_server, {player_actions, UserPid, Actions}).

stop_player_actions_processing(UserPid) ->
  gen_server:cast(bistrotanks_server, {player_stopped, UserPid}).

kill_player(BulletRef, KillerPid, KilledPlayer, LastCoord) ->
  Msg = {player_killed, [{bullet, BulletRef},
                         {coordinates, LastCoord},
                         {killer, KillerPid},
                         {player, KilledPlayer}]},
  gen_server:cast(bistrotanks_server, Msg).

update_bullet(BulletRef, NewCoord) ->
  gen_server:cast(bistrotanks_server, {update_bullet, BulletRef, NewCoord}).

kill_bullet(BulletRef, LastCoord) ->
  gen_server:cast(bistrotanks_server, {kill_bullet, BulletRef, LastCoord}).

%% gen_server callbacks
init([]) ->
  {ok, #world_status{}}.

handle_call({register_new_user, Pid, RemoteIp}, _From, State) ->
  Alive = erlang:is_process_alive(Pid),
  if (Alive =:= true) ->
    erlang:monitor(process, Pid),
    User = new_user(RemoteIp),
    Spectators = maps:put(Pid, User, State#world_status.spectators),
    message_broker:add_to_queue([messages:user_connected(User),
                                 messages:number_of_users(maps:size(Spectators), maps:size(State#world_status.players))]),
    Reply = msgpack:pack(messages:user_registered(User, maps:values(State#world_status.players))),
    {reply, {ok, Reply}, State#world_status{spectators = Spectators}};
    true -> {reply, websocket_is_dead, State}
  end;
handle_call(get_player_names, _From, State) ->
  PlayerNames = lists:map(fun(U) -> U#user.name end, maps:values(State#world_status.players)),
  {reply, {ok, PlayerNames}, State};
handle_call(Message, _From, State) ->
  lager:info("handle_call: ~p", [Message]),
  {reply, ok, State}.

handle_cast({join_game, UserPid, Name}, State) ->
  {ok, User} = maps:find(UserPid, State#world_status.spectators),
  UserWithColor = set_random_color(User),
  UpdatedUser = set_random_position(UserWithColor#user{status = player, name = Name, tank_status = newborn}),
  Spectators = maps:remove(UserPid, State#world_status.spectators),
  Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
  message_broker:add_to_queue([messages:user_joined_game(UpdatedUser),
                               messages:number_of_users(maps:size(Spectators), maps:size(Players))]),
  UpdatedState = State#world_status{players = Players, spectators = Spectators},
  erlang:send_after(?PLAYER_NEWBORN_TIME, self(), {tank_grew_up, UserPid}),
  {noreply, UpdatedState};
handle_cast({leave_game, UserPid}, State) ->
  {ok, User} = maps:find(UserPid, State#world_status.players),
  UpdatedUser = User#user{status = spectator, kills = 0, deaths = 0},
  Spectators = maps:put(UserPid, UpdatedUser, State#world_status.spectators),
  Players = maps:remove(UserPid, State#world_status.players),
  %% notify bullets
  [bullet:remove_player(BulledPid, UserPid) || BulledPid <- State#world_status.bullets],
  message_broker:add_to_queue([messages:user_left_game(User),
                               messages:number_of_users(maps:size(Spectators), maps:size(Players))]),
  UpdatedState = State#world_status{players = Players, spectators = Spectators},
  {noreply, UpdatedState};

%% handle actions by player
handle_cast({player_actions, UserPid, Actions}, State) ->
  {ok, User} = maps:find(UserPid, State#world_status.players),
  OtherPlayers = maps:remove(UserPid, State#world_status.players),

  UpdatedUser = handle_player_action(Actions, User, maps:values(OtherPlayers)),
  Players = maps:put(UserPid, UpdatedUser#user{position_updated = true}, State#world_status.players),

  %% notify bullets about updated position
  [bullet:update_player(BulledPid, UserPid, [UpdatedUser#user.x, UpdatedUser#user.y]) || BulledPid <- State#world_status.bullets],

  %% check if user wants to fire
  NeedToFire = lists:any(fun(A)-> A == "fire" end, Actions),
  Bullets = if NeedToFire =:= true ->
              BulletPid = create_bullet(UserPid, UpdatedUser, Players),
              message_broker:add_to_queue(messages:bullet_fired(bullet:get_uuid(BulletPid), UpdatedUser)),
              [BulletPid|State#world_status.bullets];
              true -> State#world_status.bullets
            end,

  message_broker:add_to_queue(messages:user_position(UpdatedUser)),
  {noreply, State#world_status{players = Players, bullets = Bullets}};
%% no actions from player, do not include him in world updates
handle_cast({player_stopped, UserPid}, State) ->
%%   lager:info("player stopped"),
  {ok, User} = maps:find(UserPid, State#world_status.players),
  UpdatedUser = User#user{position_updated = false},
  Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
  {noreply, State#world_status{players = Players}};

%% bullets
handle_cast({update_bullet, {_BulletPid, BulletUUID}, Coord}, State) ->
  message_broker:add_to_queue(messages:bullet_position(BulletUUID, Coord)),
  {noreply, State};
handle_cast({kill_bullet, {BulletPid, BulletUUID}, LastCoord}, State) ->
  message_broker:add_to_queue([messages:bullet_position(BulletUUID, LastCoord),
                               messages:bullet_dead(BulletUUID)]),
  Bullets = State#world_status.bullets -- [BulletPid],
  {noreply, State#world_status{bullets = Bullets}};
handle_cast({player_killed, [{bullet, BulletRef}, {coordinates, LastCoord}, {killer, KillerPid}, {player, KilledPid}]}, State) ->
  {BulletPid, BulletUUID} = BulletRef,
  %% killer may not be in game anymore
  {KillerUUID, Players} = case maps:find(KillerPid, State#world_status.players) of
                            {ok, UserKiller} ->
                              UserKilledUpdated = UserKiller#user{kills = UserKiller#user.kills + 1},
                              {UserKiller#user.uuid, maps:put(KillerPid, UserKilledUpdated, State#world_status.players)};
                            _ ->
                              {"unknown", State#world_status.players}
                          end,
  {ok, KilledUser} = maps:find(KilledPid, Players),
  erlang:send_after(?RESPAWN_AFTER, whereis(bistrotanks_server), {respawn, KilledPid}),
  KilledUserUpdated = KilledUser#user{tank_status = dead, deaths = KilledUser#user.deaths + 1},
  UpdatedPlayers = maps:put(KilledPid, KilledUserUpdated, Players),
  Bullets = State#world_status.bullets -- [BulletPid],
  message_broker:add_to_queue([messages:bullet_position(BulletUUID, LastCoord),
                               messages:user_killed(KilledUserUpdated, KillerUUID),
                               messages:bullet_dead(BulletUUID)]),
  {noreply, State#world_status{bullets = Bullets, players = UpdatedPlayers}};

handle_cast(Message, State) ->
  lager:info("handle_cast, unknown message: ~p", [Message]),
  {noreply, State}.

%% client disconnected
handle_info({'DOWN', MonitorRef, process, UserPid, _Cause}, State) ->
  erlang:demonitor(MonitorRef),
  Users = maps:merge(State#world_status.players, State#world_status.spectators),
  {ok, User} = maps:find(UserPid, Users),
  {Players, Spectators} = if User#user.status =:= player ->
                            %% notify bullets
                            [bullet:remove_player(BulledPid, UserPid) || BulledPid <- State#world_status.bullets],
                            message_broker:add_to_queue([messages:user_left_game(User),
                                                         messages:user_disconnected(User)]),
                            {maps:remove(UserPid, State#world_status.players), State#world_status.spectators};
                            true ->
                              message_broker:add_to_queue(messages:user_disconnected(User)),
                              {State#world_status.players, maps:remove(UserPid, State#world_status.spectators)}
                          end,
  message_broker:add_to_queue(messages:number_of_users(maps:size(Spectators), maps:size(Players))),
  {noreply, State#world_status{players = Players, spectators = Spectators}};
handle_info({tank_grew_up, UserPid}, State) ->
  case maps:find(UserPid, State#world_status.players) of
    {ok, User} ->
      UpdatedUser = User#user{tank_status = mature},
      Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
      message_broker:add_to_queue(messages:tank_grew_up(UpdatedUser)),
      %% notify bullets that they can possibly hit this player
      [bullet:add_player(BulledPid, UserPid, [UpdatedUser#user.x, UpdatedUser#user.y]) || BulledPid <- State#world_status.bullets],
      {noreply, State#world_status{players = Players}};
    _ -> {noreply, State}
  end;
handle_info({respawn, UserPid}, State) ->
  case maps:find(UserPid, State#world_status.players) of
     {ok, User} ->
       UpdatedUserWithPosition = set_random_position(User),
       UpdatedUser = UpdatedUserWithPosition#user{tank_status = newborn},
       erlang:send_after(?PLAYER_NEWBORN_TIME, whereis(bistrotanks_server), {tank_grew_up, UserPid}),
       Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
       message_broker:add_to_queue(messages:respawn(UpdatedUser)),
       {noreply, State#world_status{players = Players}};
     _ -> {noreply, State}
  end;
handle_info(Message, State) ->
  lager:info("handle_info: ~p", [Message]),
  {noreply, State}.

terminate(normal, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private

new_user(RemoteIp) ->
  #user{uuid = uuid:uuid_to_string(uuid:get_v4()), ip = RemoteIp}.

set_random_position(User) ->
  XPos = ?TANK_SIZE + random:uniform(?WORLD_WIDTH - ?TANK_SIZE * 2),
  YPos = ?TANK_SIZE + random:uniform(?WORLD_HEIGHT - ?TANK_SIZE * 2),
  Rotation = random:uniform(6),
  User#user{x = XPos, y = YPos, rotation = Rotation}.

set_random_color(User) ->
  ColorId = random:uniform(tuple_size(?COLORS)),
  User#user{color = element(ColorId, ?COLORS)}.

handle_player_action([Action|Actions], User, Players) ->
  UpdatedUser = case Action of
                  "left" -> User#user{rotation = User#user.rotation - ?ROTATION_SPEED};
                  "right" -> User#user{rotation = User#user.rotation + ?ROTATION_SPEED};
                  "up" ->
                    RawX = User#user.x + x_offset(User#user.rotation),
                    RawY = User#user.y + y_offset(User#user.rotation),
                    {X, Y} = verify_coordinates(RawX, RawY, Players),
                    User#user{x = X, y = Y};
                  "down" ->
                    RawX = User#user.x - x_offset(User#user.rotation),
                    RawY = User#user.y - y_offset(User#user.rotation),
                    {X, Y} = verify_coordinates(RawX, RawY, Players),
                    User#user{x = X, y = Y};
                  "fire" ->
                    User;
                  _ -> User
                end,
  handle_player_action(Actions, UpdatedUser, Players);
handle_player_action([], User, _Players) -> User.

x_offset(Rotation) ->
  erlang:round(math:cos(Rotation) * ?TANK_SPEED).

y_offset(Rotation) ->
  erlang:round(math:sin(Rotation) * ?TANK_SPEED).

verify_coordinates(X, Y, Players) ->
  %%   check for borders first
  X1 = if X > ?WORLD_WIDTH - ?TANK_SIZE -> ?WORLD_WIDTH - ?TANK_SIZE;
         true ->
           if X < ?TANK_SIZE -> ?TANK_SIZE;
             true -> X
           end
       end,
  Y1 = if Y > ?WORLD_HEIGHT - ?TANK_SIZE -> ?WORLD_HEIGHT - ?TANK_SIZE;
         true ->
           if Y < ?TANK_SIZE -> ?TANK_SIZE;
             true -> Y
           end
       end,
  %%   check for collisions with other players
  ConflictPlayers = lists:filter(fun(P) ->
                      (X1 >= P#user.x - ?TANK_SIZE) and
                        (X1 =< P#user.x + ?TANK_SIZE) and
                        (Y1 >= P#user.y - ?TANK_SIZE) and
                        (Y1 =< P#user.y + ?TANK_SIZE)
                    end, Players),
  if length(ConflictPlayers) > 0 ->
    [ConflictPlayer|_] = ConflictPlayers,
    X2 = if ConflictPlayer#user.x > X1 -> ConflictPlayer#user.x - (?TANK_SIZE);
           true -> ConflictPlayer#user.x + (?TANK_SIZE)
         end,
    Y2 = if ConflictPlayer#user.y > Y1 -> ConflictPlayer#user.y - (?TANK_SIZE);
           true -> ConflictPlayer#user.y + (?TANK_SIZE)
         end,
    {X2, Y2};
    true -> {X1, Y1}
  end.

create_bullet(UserPid, UserDetails, Players) ->
  MaturePlayers = maps:filter(fun(_K, P) -> P#user.tank_status =:= mature end, Players),
  CurrentPlayers = maps:map(fun(_K, P) -> [P#user.x, P#user.y] end, MaturePlayers),
  bullet:start_link([{user, UserPid}, {coordinates, [UserDetails#user.x, UserDetails#user.y, UserDetails#user.rotation]}, {players, CurrentPlayers}]).
