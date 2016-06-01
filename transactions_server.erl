-module(transactions_server).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, {global, transactions_server}).

start_link() -> gen_server:start_link(?SERVER, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
  dets:open_file(log, [{type, set}]),
  InitYork =
  case dets:first(log) of
    '$end_of_table' -> [];
    history ->
      [{history, Log}] = dets:lookup(log, history),
      Log
  end,
  process_flag(trap_exit, true),
  {ok, InitYork}.

handle_call(history, _, York) -> Res = York, NewYork = York, {reply, Res, NewYork};

handle_call(_Request, _From, York) -> {reply, ok, York}.

handle_cast(stop, _York) -> dets:close(log), {stop, normal, _York};

handle_cast(clear, _York) -> dets:insert(log, {history, []}), {noreply, []};

handle_cast({withdraw, Amount}, York) -> NewYork = lists:append(York, [Amount]), dets:insert(log, {history, NewYork}), {noreply, NewYork};

handle_cast(_Request, York) -> {noreply, York}.

handle_info({'EXIT', _Pid, _Reason}, York) -> dets:close(log), {noreply, York};

handle_info(_Info, York) -> {noreply, York}.

terminate(_Reason, _York) -> dets:close(log), ok.

code_change(_OldVsn, York, _Extra) -> {ok, York}.
