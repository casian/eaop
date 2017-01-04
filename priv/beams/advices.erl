-module(advices).
-compile(export_all).

before_advice(Type, Pid, Module, Function, Payload) ->
  io:format("~nBEFORE: {~p,~p,~p,~p,~p}", [Type, Pid, Module, Function, Payload]).

after_advice(Type, Pid, Module, Function, Payload) ->
  io:format("~nAFTER: {~p,~p,~p,~p,~p}", [Type, Pid, Module, Function, Payload]),
  lists:last(lists:last(Payload)).

override_advice(Type, Pid, Module, Function, Payload) ->
  io:format("~nOVERRIDE: {~p,~p,~p,~p,~p}", [Type, Pid, Module, Function, Payload]),
  case Type of function ->
    [Mod, Fun, Args] = Payload,

    case {Module, Function, length(Args)} of
      {server, start, 0} ->
        io:format("~nStart function OVERRIDDEN."),
        apply(Mod, Fun, Args);
      _ -> ok
    end;
    _ -> ok
  end.

intercept_advice(Type, Pid, Module, Function, Payload) ->
  io:format("~nINTERCEPT: {~p,~p,~p,~p,~p}", [Type, Pid, Module, Function, Payload]),
  case Type of
    'receive' ->
      io:format("~nRECEIVE"),
      [RecvFun] = Payload, RecvFun();
    Other -> io:format("~nRECEIVE OTHER ~p", [Other]), ok
  end,

  case Payload of
    [_, spawn, [Fun]] -> spawn(fun() -> io:format("New Actor ~p", [self()]), Fun() end);
    _ -> ok
  end.

upon_advice(Type, Pid, Module, Function, Payload) ->
  io:format("~nUPON: {~p,~p,~p,~p,~p}", [Type, Pid, Module, Function, Payload]).