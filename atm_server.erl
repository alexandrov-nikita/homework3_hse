-module(atm_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SERVER, {global, atm_server}).

start_link() -> gen_server:start_link(?SERVER, ?MODULE, [], []).

init([]) -> Initial = [5000, 50, 50, 50, 1000, 5000, 1000, 500, 100], {ok, Initial}.

handle_call({withdraw, Amount}, _, Now) -> {Status, Result, Remained} = atm:withdraw(Amount, Now),
  if
    Status==ok -> gen_server:cast({global, transactions_server}, {withdraw, Amount});
    true -> do_nothing
  end,
  {reply, {Status, Result}, Remained};

handle_call(_Request, _From, Now) -> {reply, ok, Now}.

handle_cast(_Request, Now) -> {noreply, Now}.

handle_info(_Info, Now) -> {noreply, Now}.

code_change(_OldVsn, Now, _Extra) -> {ok, Now}.

terminate(_Reason, _Now) -> ok.