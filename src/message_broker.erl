-module(message_broker).
-behaviour(gen_server).
-include("records.hrl").

-export([add_to_queue/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
add_to_queue(Messages) when is_list(Messages) ->
  gen_server:cast(message_broker, {add_to_queue, Messages});
add_to_queue(Message) ->
  add_to_queue([Message]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), send_updates),
  {ok, #broker{}}.

handle_call(Message, _From, State) ->
  lager:info("handle_call: ~p", [Message]),
  {reply, ok, State}.

handle_cast({add_listener, Pid}, State) ->
  Alive = is_process_alive(Pid),
  if Alive =:= true ->
    erlang:monitor(process, Pid),
    Listeners = [Pid|State#broker.listeners],
    {noreply, State#broker{listeners = Listeners}};
    true ->
      {noreply, State}
  end;
handle_cast({add_to_queue, Messages}, State) ->
  UpdatedMessages = State#broker.messages ++ Messages,
  {noreply, State#broker{messages = UpdatedMessages}};
handle_cast(Message, State) ->
  lager:info("handle_cast, unknown message: ~p", [Message]),
  {noreply, State}.

%% send world updates by timer, if any
handle_info(send_updates, State) when length(State#broker.messages) > 0 ->
  Message = messages:world_updates(State#broker.messages),
  [UserPid ! {world_updates, msgpack:pack(Message)} || UserPid <- State#broker.listeners],
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), send_updates),
  {noreply, State#broker{messages = []}};
handle_info(send_updates, State) ->
  erlang:send_after(?WORLD_UPDATES_INTERVAL, self(), send_updates),
  {noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, _Cause}, State) ->
  erlang:demonitor(MonitorRef),
  Listeners = State#broker.listeners -- [Pid],
  {noreply, State#broker{listeners = Listeners}};
handle_info(Message, State) ->
  lager:info("handle_info: ~p", [Message]),
  {noreply, State}.

terminate(normal, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
