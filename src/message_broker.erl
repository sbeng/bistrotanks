-module(message_broker).
-include("records.hrl").

%% API
-compile(export_all).

user_registered(User, CurrentPlayers) when is_record(User, user) ->
  #{<<"type">> => <<"registered">>,
    <<"data">> => #{<<"color">> => atom_to_binary(User#user.color, unicode),
                    <<"userId">> => list_to_binary(User#user.uuid),
                    <<"players">> => lists:map(fun(P) -> user_info(P) end, CurrentPlayers)
    }
  }.

user_connected(User) ->
  #{<<"type">> => <<"connected">>,
    <<"data">> => #{<<"id">> => list_to_binary(User#user.uuid),
                    <<"ip">> => list_to_binary(User#user.ip)}}.

user_disconnected(User) ->
  #{<<"type">> => <<"disconnected">>,
    <<"data">> => #{<<"id">> => list_to_binary(User#user.uuid),
                    <<"ip">> => list_to_binary(User#user.ip)}}.

number_of_users(Spectators, Players) ->
  #{<<"type">> => <<"number_of_users">>,
    <<"data">> => #{<<"players">> => Players,
                    <<"spectators">> => Spectators}}.
world_updates(Updates) ->
  #{<<"type">> => <<"world_updates">>, <<"data">> => Updates}.

user_joined_game(User) ->
  #{<<"type">> => <<"joined_game">>,
    <<"data">> => user_info(User)}.

user_left_game(User) ->
  #{<<"type">> => <<"left_game">>,
    <<"data">> => #{<<"id">> => list_to_binary(User#user.uuid), <<"name">> => User#user.name,
                    <<"color">> => atom_to_binary(User#user.color, unicode)}}.

user_position(User) ->
  #{<<"type">> => <<"updated_position">>,
    <<"data">> => #{<<"x">>  => User#user.x, <<"y">> => User#user.y,
    <<"rotation">> => User#user.rotation, <<"id">> => list_to_binary(User#user.uuid)}}.

name_invalid() ->
  #{<<"type">> => <<"failed_to_join">>, <<"reason">> => <<"Name is invalid">>}.

name_already_taken() ->
  #{<<"type">> => <<"failed_to_join">>, <<"reason">> => <<"Name is already taken">>}.

tank_grew_up(User) ->
  #{<<"type">> => <<"tank_grew_up">>,
    <<"data">> => #{<<"id">> => list_to_binary(User#user.uuid)}}.

respawn(User) ->
  #{<<"type">> => <<"respawn">>,
    <<"data">> => #{<<"id">> => list_to_binary(User#user.uuid), <<"x">> => User#user.x,
                    <<"y">> => User#user.y, <<"rotation">> => User#user.rotation}}.

bullet_fired(BulletUUID, User) ->
  #{<<"type">> => <<"bullet_fired">>,
    <<"data">> => #{<<"id">> => list_to_binary(BulletUUID), <<"x">> => User#user.x,
                    <<"y">> => User#user.y, <<"rotation">> => User#user.rotation}}.

bullet_position(BulletUUID, {X, Y}) ->
  #{<<"type">> => <<"bullet_position">>,
    <<"data">> => #{<<"id">> => list_to_binary(BulletUUID), <<"x">> => X,
                    <<"y">> => Y}}.

bullet_dead(BulletUUID) ->
  #{<<"type">> => <<"bullet_dead">>,
    <<"data">> => #{<<"id">> => list_to_binary(BulletUUID)}}.

user_killed(KilledUser, KillerUUID) ->
  #{<<"type">> => <<"user_killed">>,
    <<"data">> => #{<<"killed_id">> => list_to_binary(KilledUser#user.uuid),
                    <<"killer_id">> => list_to_binary(KillerUUID)}}.

%% private
user_info(User) ->
  #{<<"x">>  => User#user.x, <<"y">> => User#user.y,
    <<"rotation">> => User#user.rotation,
    <<"id">> => list_to_binary(User#user.uuid),
    <<"name">> => User#user.name,
    <<"kills">> => User#user.kills,
    <<"deaths">> => User#user.deaths,
    <<"color">> => atom_to_binary(User#user.color, unicode),
    <<"tank_status">> => atom_to_binary(User#user.tank_status, unicode)}.
