-define(COLORS, {green, red, yellow, blue, purple, lightblue, orange, pink}).
-define(STATUSES, {newborn, mature, dead}).
-define(WORLD_UPDATES_INTERVAL, 40). % send world updates every 40 milliseconds
-define(PLAYER_ACTIONS_INTERVAL, 80). % check for actions of player every 80 milliseconds
-define(PLAYER_NEWBORN_TIME, 3000). % first 3 seconds of game player has newborn status
-define(PLAYER_ACTIONS_TIMEOUT, 20). % After 20 seconds of inactivity player will be moved to spectators
-define(ROTATION_SPEED, 0.3). % in radians, not degrees
-define(TANK_SPEED, 10).
-define(BULLET_SPEED, ?TANK_SPEED * 2). % bullet is twice faster
-define(WORLD_WIDTH, 737). % px
-define(WORLD_HEIGHT, 545). % px
-define(TANK_SIZE, 16). % px
-define(TANK_FIRE_RATE, 0.3). % 1 fire per 0.3 sec
-define(RESPAWN_AFTER, 2000). % 3 seconds



-record(user, {name = <<>>, kills = 0, deaths = 0, color, status = spectator,
               uuid, ip, x = 0, y = 0, rotation = 0, position_updated = false, tank_status = newborn}).
-record(world_status, {spectators = #{}, players = #{}, updates = [], bullets = []}).
-record(bullet_status, {x = 0, y = 0, rotation = 0, user_fired, players = #{}, uuid}).
-record(connection, {connection_status = spectator, player_actions = [],
                     last_action_at, is_moving = false, last_fire_at}).
