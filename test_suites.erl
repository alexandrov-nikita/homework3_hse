%% Testing suite for 0.4, 0.6, 0.7, 0.9 and 1.0 scores
%% For bonus scores there will be separate tests
%% ======================= AGREEMENTS ==============================================
% 1. For 0.6 score 'withdraw' function defined in module 'atm'
% 2. For 0.7 score there is 'atm_server' module implemented as gen_server
%    Server initial state (IMPORTANT!!!): [5000, 50, 50, 50, 1000, 5000, 1000, 500, 100]
% 3. For 0.8 score there also 'atm_sup' module is implemented as gen_supervisor, and
%    main server is registered as {global, atm_server}:
-define(ATM_SERVER, {global, atm_server}).
%    This server accepts operations:
%      - [CALL] {withdraw, Amount}
% 4. For 0.9 score there is 'transactions_server' module implemented as gen_server:
-define(PROCESSING_CENTER, {global, transactions_server}).
%    This server accept operations:
%      - [CAST] {withdraw, Amount}
%      - [CAST] clear
%      - [CALL] history -> []
% 5. For 1.0 score transactions server should store it's state and to be supervised
%    by the same supervisor 'atm_sup'

-module(test_suites).
-export([test_suite_0_6/0, test_suite_0_7/0, test_suite_0_8/0, test_suite_0_9/0, test_suite_1_0/0]).
-export([main/1]).

test_suite_0_6() ->
  State0 = [5000, 50, 50, 50, 1000, 5000, 1000, 500, 100],
  {ok, [5000], State1} = atm:withdraw(5000, State0),
  {ok, [5000, 1000], State2} = atm:withdraw(6000, State1),
  {request_another_amount, [], State2} = atm:withdraw(6000, State2),
  {ok, [], State2} = atm:withdraw(0, State2),
  {ok, [1000, 500, 100, 50, 50, 50], State3} = atm:withdraw(1750, State2),
  ok = try
         atm:withdraw(100, State3),
         error_not_raised
       catch
         _: _ -> ok
       end,
  io:format("Tests for 0.6 passed!~n").


interact_with_gen_server(Server) ->
  {ok, [5000]} = gen_server:call(Server, {withdraw, 5000}),
  {ok, [5000, 1000]} = gen_server:call(Server, {withdraw, 6000}),
  {request_another_amount, []} = gen_server:call(Server, {withdraw, 6000}),
  {ok, []} = gen_server:call(Server, {withdraw, 0}),
  {ok, [1000, 500, 100, 50, 50, 50]} = gen_server:call(Server, {withdraw, 1750}),
  ok = try
         gen_server:call(Server, {withdraw, 100}),
         error_not_raised
       catch
         exit : _ -> ok
       end,
  ok.

test_suite_0_7() ->
  % 0.7 score assumes 0.6 score passed
  test_suite_0_6(),

  Server = {global, my_atm_instance},

  {ok, _} = gen_server:start_link(Server, atm_server, [], []),
  interact_with_gen_server(Server),

  % Try again with new instance
  {ok, _} = gen_server:start_link(Server, atm_server, [], []),
  interact_with_gen_server(Server),
  io:format("Tests for 0.7 passed!~n").

interact_with_next_gen_server(0) -> ok;
interact_with_next_gen_server(N) ->
  interact_with_gen_server(?ATM_SERVER),

  %% Give some tome for supervisor to restart (50ms is sure enought)
  timer:sleep(50),
  interact_with_next_gen_server(N-1).

test_suite_0_8() ->
  % 0.8 score assumes 0.6 score passed
  test_suite_0_6(),

  {ok, _}  = atm_sup:start_link(),
  interact_with_next_gen_server(5),

  io:format("Tests for 0.7 passed!~n").

check_history(History) ->
  % There are two correct answers: 'withdraw 0' processed or not
  case History of
    [5000, 6000, 0, 1750] -> ok;
    [5000, 6000, 1750] -> ok;
    _ -> wrong_history
  end.

test_suite_0_9() ->
  % 0.9 score assumes 0.8 score passed because it requires supervisor tree
  test_suite_0_8(),

  % Really not start supervisor tree due to already started in 0.8 test suite
  % {ok, _} = atm_sup:start_link(),

  % Clear all history
  gen_server:cast(?PROCESSING_CENTER, clear),

  % Make some operations like previous test case...
  interact_with_next_gen_server(1),

  % Match history
  History = gen_server:call(?PROCESSING_CENTER, history),
  ok = check_history(History),
  io:format("Tests for 0.9 passed!~n").

test_suite_1_0() ->
  % 1.0 score assumes 0.9 score is passed
  test_suite_0_9(),

  % Forget everything
  gen_server:stop(?PROCESSING_CENTER),

  % Restart processing center not required in case of it restarted by supervisor rule.
  % Just wait for I/O operations (1 sec is more than enought event for slow disks)
  timer:sleep(1000),

  % Match history again - it should be the same before restart
  History = gen_server:call(?PROCESSING_CENTER, history),
  ok = check_history(History),
  io:format("Tests for 1.0 passed!~n").

main(Args) ->
  [TestName | _] = Args,
  case TestName of
    "0.6" -> test_suite_0_6();
    "0.7" -> test_suite_0_7();
    "0.8" -> test_suite_0_8();
    "0.9" -> test_suite_0_9();
    "1.0" -> test_suite_1_0();
    _ -> io:format("Possible tests: 0.6, 0.7, 0.8, 0.9, 1.0~n")
  end.
