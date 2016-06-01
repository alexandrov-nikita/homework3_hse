-module(atm).
-export([withdraw/2, withdraw/3]).

withdraw(_Cash, []) -> exit(normal);

withdraw(Cash, Banknotes) -> withdraw(Cash, lists:reverse(lists:sort(Banknotes)), do).

withdraw(0, Banknotes, do) -> {ok, [], Banknotes};

withdraw(_Cash, [], do) -> {request_another_amount, [], []};

withdraw(Cash, Banknotes, do) ->
  [CurrentBanknote|Rest] = Banknotes,
  {Res, RestBanknote} =
    if
      CurrentBanknote > Cash -> CurrentBalance = 0, {[], [CurrentBanknote]};
      true -> CurrentBalance = CurrentBanknote, {[CurrentBanknote], []}
    end,

    {Status, Result, RemainingBanknotes} = withdraw(Cash - CurrentBalance, Rest, do),

    if Status == ok -> {ok, lists:append(Res, Result), lists:append(RestBanknote, RemainingBanknotes)};
      Status == request_another_amount ->  {request_another_amount, [], Banknotes}
    end.



