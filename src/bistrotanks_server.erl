-module(bistrotanks_server).
-behaviour(gen_server).
-include("records.hrl").

-export([player_names/0, join_game/2, leave_game/1, add_spectator/2, process_player_actions/2,
         stop_player_actions_processing/1, kill_player/4, kill_bullet/2, update_bullet/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
%% TODO: move API to bistrotanks module?
player_names() ->
  gen_server:call(bistrotanks_server, get_player_names).

join_game(UserPid, UserName) ->
  gen_server:cast(bistrotanks_server, {join_game, UserPid, UserName}),
  erlang:send_after(?PLAYER_NEWBORN_TIME, whereis(bistrotanks_server), {tank_grew_up, UserPid}).

leave_game(UserPid) ->
  gen_server:cast(bistrotanks_server, {leave_game, UserPid}).

add_spectator(UserPid, UserIp) ->
  gen_server:call(bistrotanks_server, {register_new_user, UserPid, UserIp}).

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
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), update_the_world),
  {ok, #world_status{}}.

handle_call({register_new_user, Pid, RemoteIp}, _From, State) ->
  erlang:monitor(process, Pid),
  User = new_user(RemoteIp),
  Spectators = maps:put(Pid, User, State#world_status.spectators),
  Reply = {ok, msgpack:pack(message_broker:user_registered(User, maps:values(State#world_status.players)))},
  Messages = [message_broker:user_connected(User),
              message_broker:number_of_users(maps:size(Spectators), maps:size(State#world_status.players))],
  Updates = State#world_status.updates ++ Messages,
  {reply, Reply, State#world_status{spectators = Spectators, updates = Updates}};
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
  Messages = [message_broker:user_joined_game(UpdatedUser),
              message_broker:number_of_users(maps:size(Spectators), maps:size(Players))],
  Updates = State#world_status.updates ++ Messages,
  UpdatedState = State#world_status{players = Players, spectators = Spectators, updates = Updates},
  {noreply, UpdatedState};
handle_cast({leave_game, UserPid}, State) ->
  {ok, User} = maps:find(UserPid, State#world_status.players),
  UpdatedUser = User#user{status = spectator, kills = 0, deaths = 0},
  Spectators = maps:put(UserPid, UpdatedUser, State#world_status.spectators),
  Players = maps:remove(UserPid, State#world_status.players),
  %% notify bullets
  [bullet:remove_player(BulledPid, UserPid) || BulledPid <- State#world_status.bullets],
  Messages = [message_broker:user_left_game(User),
              message_broker:number_of_users(maps:size(Spectators), maps:size(Players))],
  Updates = State#world_status.updates ++ Messages,
  UpdatedState = State#world_status{players = Players, spectators = Spectators, updates = Updates},
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
  {Bullets, Messages} = if NeedToFire =:= true ->
              BulletPid = create_bullet(UserPid, UpdatedUser, Players),
              {[BulletPid|State#world_status.bullets], State#world_status.updates ++ [message_broker:bullet_fired(bullet:get_uuid(BulletPid), UpdatedUser)]};
              true -> {State#world_status.bullets, State#world_status.updates}
            end,

  Updates = Messages ++ [message_broker:user_position(UpdatedUser)],
  {noreply, State#world_status{players = Players, updates = Updates, bullets = Bullets}};
%% no actions from player, do not include him in world updates
handle_cast({player_stopped, UserPid}, State) ->
%%   lager:info("player stopped"),
  {ok, User} = maps:find(UserPid, State#world_status.players),
  UpdatedUser = User#user{position_updated = false},
  Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
  {noreply, State#world_status{players = Players}};

%% bullets
handle_cast({update_bullet, {_BulletPid, BulletUUID}, Coord}, State) ->
  Updates = State#world_status.updates ++ [message_broker:bullet_position(BulletUUID, Coord)],
  {noreply, State#world_status{updates = Updates}};
handle_cast({kill_bullet, {BulletPid, BulletUUID}, LastCoord}, State) ->
  Updates = State#world_status.updates ++ [message_broker:bullet_position(BulletUUID, LastCoord),
                                           message_broker:bullet_dead(BulletUUID)],
  Bullets = State#world_status.bullets -- [BulletPid],
  {noreply, State#world_status{updates = Updates, bullets = Bullets}};
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
  Updates = State#world_status.updates ++ [message_broker:bullet_position(BulletUUID, LastCoord),
                                           message_broker:user_killed(KilledUserUpdated, KillerUUID),
                                           message_broker:bullet_dead(BulletUUID)],
  {noreply, State#world_status{updates = Updates, bullets = Bullets, players = UpdatedPlayers}};

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
                            {maps:remove(UserPid, State#world_status.players), State#world_status.spectators};
                            true ->
                              {State#world_status.players, maps:remove(UserPid, State#world_status.spectators)}
                          end,
  Msg = if User#user.status =:= spectator ->
          [message_broker:user_disconnected(User)];
          true ->
            [message_broker:user_left_game(User), message_broker:user_disconnected(User)]
        end,
  Messages = Msg ++ [message_broker:number_of_users(maps:size(Spectators), maps:size(Players))],
  Updates = State#world_status.updates ++ Messages,
  {noreply, State#world_status{players = Players, spectators = Spectators, updates = Updates}};
%% send world updates by timer, if any
handle_info(update_the_world, State) when length(State#world_status.updates) > 0 ->
  UserPids = maps:keys(State#world_status.players) ++ maps:keys(State#world_status.spectators),
  Message = message_broker:world_updates(State#world_status.updates),
  [UserPid ! {world_updates, msgpack:pack(Message)} || UserPid <- UserPids],
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), update_the_world),
  {noreply, State#world_status{updates = []}};
handle_info(update_the_world, State) ->
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), update_the_world),
  {noreply, State};
handle_info({tank_grew_up, UserPid}, State) ->
  UpdatedState = case maps:find(UserPid, State#world_status.players) of
                   {ok, User} ->
                     UpdatedUser = User#user{tank_status = mature},
                     Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
                     Updates = State#world_status.updates ++ [message_broker:tank_grew_up(UpdatedUser)],
                     %% notify bullets that they can possibly hit this player
                     [bullet:add_player(BulledPid, UserPid, [UpdatedUser#user.x, UpdatedUser#user.y]) || BulledPid <- State#world_status.bullets],
                     State#world_status{players = Players, updates = Updates};
                   _ -> State
                 end,
  {noreply, UpdatedState};
handle_info({respawn, UserPid}, State) ->
  UpdatedState = case maps:find(UserPid, State#world_status.players) of
                   {ok, User} ->
                     UpdatedUserWithPosition = set_random_position(User),
                     UpdatedUser = UpdatedUserWithPosition#user{tank_status = newborn},
                     erlang:send_after(?PLAYER_NEWBORN_TIME, whereis(bistrotanks_server), {tank_grew_up, UserPid}),
                     Players = maps:put(UserPid, UpdatedUser, State#world_status.players),
                     Updates = State#world_status.updates ++ [message_broker:respawn(UpdatedUser)],
                     State#world_status{players = Players, updates = Updates};
                   _ -> State
                end,
  {noreply, UpdatedState};
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
