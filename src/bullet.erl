-module(bullet).
-behaviour(gen_server).
-include("records.hrl").

%% API
-export([remove_player/2, update_player/3, add_player/3, get_uuid/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link([{user, UserFiredPid}, {coordinates, [X, Y, Rotation]}, {players, CurrentPlayers}]) ->
  State = #bullet_status{x = X, y = Y, rotation = Rotation, user_fired = UserFiredPid, players = CurrentPlayers},
  {ok, Pid} = gen_server:start_link(?MODULE, State, []),
  Pid.

%% API
add_player(BulledPid, UserPid, Coordinates) ->
  BulledPid ! {add_player, [{user, UserPid}, {coordinates, Coordinates}]}.

remove_player(BulledPid, UserPid) ->
  BulledPid ! {remove_player, UserPid}.

update_player(BulledPid, UserPid, Coordinates) ->
  BulledPid ! {player_moved, [{user, UserPid}, {coordinates, Coordinates}]}.

get_uuid(BulledPid) ->
  gen_server:call(BulledPid, get_uuid).

%% gen_server callbacks
init(State) ->
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), update_own_position),
  {ok, State#bullet_status{uuid = uuid:uuid_to_string(uuid:get_v4())}}.

handle_call(get_uuid, _From, State) ->
  {reply, State#bullet_status.uuid, State};
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(update_own_position, State) ->
  LineStart = {State#bullet_status.x, State#bullet_status.y},
  LineEnd = {State#bullet_status.x + x_offset(State#bullet_status.rotation), State#bullet_status.y + y_offset(State#bullet_status.rotation)},
  %%   check for players to hit
  FoldFun = fun(Pid, [PlayerX, PlayerY], Acc) ->
    PolygonStart = {PlayerX - ?TANK_SIZE / 2, PlayerY - ?TANK_SIZE / 2},
    PolygonEnd = {PlayerX + ?TANK_SIZE / 2, PlayerY + ?TANK_SIZE / 2},
    PolygonPoints = [PolygonStart, {element(1, PolygonStart), element(2, PolygonEnd)},
                     PolygonEnd, {element(1, PolygonEnd), element(2, PolygonStart)}],
    AllPositive = lists:all(fun(Point)-> intersection_check(Point, LineStart, LineEnd) > 0 end, PolygonPoints),
    AllNegative = lists:all(fun(Point)-> intersection_check(Point, LineStart, LineEnd) < 0 end, PolygonPoints),
    if AllPositive or AllNegative or (Pid == State#bullet_status.user_fired) -> Acc; % no intersection
      true ->
        if ((element(1, LineStart) > element(1, PolygonEnd)) and (element(1, LineEnd) > element(1, PolygonEnd))) or % no intersection (line is to right of rectangle).
          ((element(1, LineStart) < element(1, PolygonStart)) and (element(1, LineEnd) < element(1, PolygonStart))) or % no intersection (line is to left of rectangle).
          ((element(2, LineStart) > element(2, PolygonEnd)) and (element(2, LineEnd) > element(2, PolygonEnd))) or % no intersection (line is above rectangle).
          ((element(2, LineStart) < element(2, PolygonStart)) and (element(2, LineEnd) < element(2, PolygonStart))) -> Acc; % no intersection (line is below rectangle).
          true -> [Pid|Acc]
        end
    end
  end,
  KilledPlayers = maps:fold(FoldFun, [], State#bullet_status.players),
  {RawX, RawY} = LineEnd,
  if length(KilledPlayers) > 0 ->
    [KilledPid|_] = KilledPlayers,
    bistrotanks_server:kill_player({self(), State#bullet_status.uuid},
                                   State#bullet_status.user_fired,
                                   KilledPid, {RawX, RawY}),
    {stop, normal, State};
    true ->
      if (RawX > ?WORLD_WIDTH) or (RawX < 0) or (RawY > ?WORLD_HEIGHT) or (RawY < 0) ->
        BulletX = if (RawX > ?WORLD_WIDTH) -> ?WORLD_WIDTH;
                    true -> 0
                  end,
        BulletY = if (RawY > ?WORLD_HEIGHT) -> ?WORLD_HEIGHT;
                    true -> 0
                  end,
        bistrotanks_server:kill_bullet({self(), State#bullet_status.uuid}, {BulletX, BulletY}),
        {stop, normal, State};
        true -> %% continue to fly
          bistrotanks_server:update_bullet({self(), State#bullet_status.uuid}, {RawX, RawY}),
          UpdatedState = State#bullet_status{x = RawX, y = RawY},
          erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), update_own_position),
          {noreply, UpdatedState}
      end
  end;
handle_info({add_player, [{user, UserPid}, {coordinates, Coord}]}, State) ->
  Players = maps:put(UserPid, Coord, State#bullet_status.players),
  {noreply, State#bullet_status{players = Players}};
handle_info({remove_player, UserPid}, State) ->
  Players = maps:remove(UserPid, State#bullet_status.players),
  {noreply, State#bullet_status{players = Players}};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(normal, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

x_offset(Rotation) ->
  erlang:round(math:cos(Rotation) * ?BULLET_SPEED).

y_offset(Rotation) ->
  erlang:round(math:sin(Rotation) * ?BULLET_SPEED).

intersection_check({X, Y}, {X1, Y1}, {X2, Y2}) ->
  (Y2 - Y1) * X +
    (X1 - X2) * Y +
    (X2 * Y1 - X1 * Y2).
