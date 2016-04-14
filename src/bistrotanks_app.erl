-module(bistrotanks_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/css/[...]", cowboy_static, {priv_dir, bistrotanks, "css", [{mimetypes, cow_mimetypes, web}]}},
      {"/js/[...]", cowboy_static, {priv_dir, bistrotanks, "js", [{mimetypes, cow_mimetypes, web}]}},
      {"/images/[...]", cowboy_static, {priv_dir, bistrotanks, "images", [{mimetypes, cow_mimetypes, web}]}},
      {"/fonts/[...]", cowboy_static, {priv_dir, bistrotanks, "fonts", [{mimetypes, cow_mimetypes, web}]}},

      {"/", index_handler, []},
      {"/websocket", websocket_handler, []},
      {'_', notfound_handler, []}
    ]}
  ]),

  {ok, _Pid} = cowboy:start_http(bistrotanks_web_server, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	bistrotanks_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(bistrotanks_web_server),
	ok.
