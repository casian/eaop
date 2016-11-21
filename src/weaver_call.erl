-define(BIFS, [spawn, link, unlink, register, unregister]).
-define(SPAWNS, [spawn_opt, spawn, spawn_link, spawn_monitor]).
-define(SPAWN_DICT, spawn_dict).

-module(weaver_call).

-include("aop.hrl").

-export([handle_call/2]).

-import(proplists, []).
-import(re, []).
-import(lists, []).
-import(string, []).
-import(fun_proxy, []).


handle_call(Forms, Defs) ->
  Aspects =  filter(Defs, call, []),
  case Aspects of
    [] -> Forms;
    _ -> process_forms_call(Forms, Aspects, [], [], [])
  end.

filter([], _, Result) -> Result;
filter([H | T], Type, Result) ->
  #pointcut{event = Event} = H,
  if Event == Type -> filter(T, Type, [H | Result]);
    true -> filter(T, Type, Result)
  end.

is_match([], _, _, _, _) -> false;
is_match([Pointcut = #pointcut{event = call, module = Module, function = Function, arity = Arity, advices = _} | T], ModuleName, FunctionName, FunctionArity, Args) ->
  ModuleCheck = (atom_to_list(ModuleName) == Module) or (Module == "_"),
  FunctionCheck = (Function == atom_to_list(FunctionName)) or (Function == "_"),
  if is_atom(FunctionArity) == true ->
    ArityCheck = (Arity == "*") or (atom_to_list(FunctionArity) == Arity),
    Check = ModuleCheck and FunctionCheck and ArityCheck,
    if Check == true ->
      {true, Pointcut};
      true ->
        is_match(T, ModuleName, FunctionName, FunctionArity, Args)
    end;
    is_integer(FunctionArity) == true ->
      ArityCheck = (Arity == "*") or (integer_to_list(FunctionArity) == Arity),
      Check = ModuleCheck and FunctionCheck and ArityCheck,
      if Check == true ->
        {true, Pointcut};
        true ->
          is_match(T, ModuleName, FunctionName, FunctionArity, Args)
      end
  end.


process_forms_call([], _, _, NewForm, _) -> lists:reverse(NewForm);
process_forms_call([{attribute, _, module, ModuleName} = H | T], Defs, _, NewForm, ArgsNumList) ->
  process_forms_call(T, Defs, ModuleName, [H | NewForm], ArgsNumList);
process_forms_call([{attribute, _, _, _} = H | T], Defs, ModuleName, NewForm, ArgsNumList) ->
  process_forms_call(T, Defs, ModuleName, [H | NewForm], ArgsNumList);
process_forms_call([{function, LineNum, FunctionName, NumArgs, Clause} | T], Defs, ModuleName, NewForm, ArgsNumList) ->
  if length(Clause) == 1 ->
    [{clause, LineNum, A, B, Contents}] = Clause,
    [Form, ArgsNumListReturned] = process_function_call(Contents, Defs, FunctionName, ModuleName, [], []),
    process_forms_call(T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, [{clause, LineNum, A, B, Form}]} | NewForm], [ArgsNumListReturned | ArgsNumList]);
    length(Clause) > 1 ->
      [NewClause, ArgsNumListReturned] = process_clauses_call(Clause, Defs, FunctionName, ModuleName, [], []),
      process_forms_call(T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, NewClause} | NewForm], [ArgsNumListReturned | ArgsNumList]);
    true -> {ok}
  end;
process_forms_call([{eof, C} | T], Defs, ModuleName, NewForm, ArgsNumList) ->
  Check = check_inject_function_call(NewForm),
  ArgsList = sets:to_list(sets:from_list(lists:flatten(ArgsNumList))),
  if (Check == true) andalso (ArgsList /= []) ->
    process_forms_call(T, Defs, ModuleName, [{eof, C} | [inject_function_call(C, ArgsList) | NewForm]], ArgsNumList);
    true ->
      process_forms_call(T, Defs, ModuleName, [{eof, C} | NewForm], ArgsNumList)
  end;
process_forms_call([_ | T], Defs, ModuleName, NewForm, ArgsNumList) ->
  process_forms_call(T, Defs, ModuleName, NewForm, ArgsNumList).

process_clauses_call([], _, _, _, NewClauses, ArgsNumListReturned) ->
  [lists:reverse(NewClauses), ArgsNumListReturned];
process_clauses_call([Clause | T], Defs, FunctionName, ModuleName, NewClauses, ArgsNumList) ->
  {clause, LineNum, A, B, Contents} = Clause,
  [NewClause | ArgsNumListReturned] = process_function_call(Contents, Defs, FunctionName, ModuleName, [], []),
  process_clauses_call(T, Defs, FunctionName, ModuleName, [{clause, LineNum, A, B, NewClause} | NewClauses], [ArgsNumListReturned | ArgsNumList]).

process_function_call([], _, _, _, NewForm, ArgsNumList) -> [lists:reverse(NewForm), ArgsNumList];
process_function_call([{call, Num, {atom, Num, FunctionCalled}, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumList) ->
   [UpdatedArgs, ArgsNumListReturned] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  CallFunctionAtom = call_@,
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, 'erlang'} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
        CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
        process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
        true ->
          NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleName} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
          CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
          process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]])
      end;
    false ->
      CallUpdated = {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_function_call([{call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumList) ->
  [UpdatedArgs, ArgsNumListReturned] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  CallFunctionAtom = call_@,
  case Match of
    {true, Pointcut} ->
      NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleCalled} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
      CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
    false ->
      CallUpdated = {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_function_call([{match, Num, A, {call, Num, {atom, Num, FunctionCalled}, Args}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumList) ->
  [UpdatedArgs, ArgsNumListReturned] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      CallFunctionAtom = call_@,
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, 'erlang'} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
        CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
        process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
        true ->
          NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleName} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
          CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
          process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]])
      end;
    false ->
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_function_call([{match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  CallFunctionAtom = call_@,
  [UpdatedArgs, ArgsNumList] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleCalled} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
    false ->
      CallUpdated = {match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;

process_function_call([{match, Num2, R, {'receive', Num, Clauses}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Clauses, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs}} | NewForm], [ArgsNumList | ArgsNumListReturned]);

process_function_call([{match, Num2, R, {'receive', Num, Clauses, OP, Clauses2}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Clauses, Defs, FunctionName, ModuleName, [], []),
  [UpdatedArgs2, ArgsNumList2] = process_case_call(Clauses2, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs, OP, UpdatedArgs2}} | NewForm], lists:concat([[ArgsNumList2, ArgsNumList] | ArgsNumListReturned]));

process_function_call([{'receive', Num, Clauses} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Clauses, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]);

process_function_call([{'receive', Num, Clauses, OP, Clauses2} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Clauses, Defs, FunctionName, ModuleName, [], []),
  [UpdatedArgs2, ArgsNumList2] = process_case_call(Clauses2, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs, OP, UpdatedArgs2} | NewForm], lists:concat([[ArgsNumList2, ArgsNumList] | ArgsNumListReturned]));

process_function_call([{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Args, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]);
process_function_call([{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_if_call(Args, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]);
process_function_call([H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  process_function_call(T, Defs, FunctionName, ModuleName, [H | NewForm], ArgsNumListReturned).

process_case_call([], _, _, _, NewForm, ArgsNumList) -> [lists:reverse(NewForm), ArgsNumList];
process_case_call([H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  {clause, A, B, C, Arg} = H,
  [UpdatedArgs, ArgsNumList] = process_arguments_call(Arg, Defs, FunctionName, ModuleName, [], []),
  process_case_call(T, Defs, FunctionName, ModuleName, [{clause, A, B, C, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]).

process_if_call([], _, _, _, NewForm, ArgsNumList) -> [lists:reverse(NewForm), ArgsNumList];
process_if_call([H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  {clause, A, B, C, Arg} = H,
  [UpdatedArgs, ArgsNumList] = process_arguments_call(Arg, Defs, FunctionName, ModuleName, [], []),
  process_if_call(T, Defs, FunctionName, ModuleName, [{clause, A, B, C, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]).


process_arguments_call([], _, _, _, NewForm, ArgsNumListReturned) -> [lists:reverse(NewForm), ArgsNumListReturned];
process_arguments_call([{call, Num, {atom, Num, FunctionCalled}, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  CallFunctionAtom = call_@,
  [UpdatedArgs | ArgsNumList] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, 'erlang'} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
        CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
        process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
        true ->
          NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleName} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
          CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
          process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]])
      end;
    false ->
      CallUpdated = {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_arguments_call([{call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  CallFunctionAtom = call_@,
  [UpdatedArgs | ArgsNumList] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleCalled} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
      CallUpdated = {call, Num, {atom, Num, CallFunctionAtom}, NewArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
    false ->
      CallUpdated = {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_arguments_call([{match, Num, A, {call, Num, {atom, Num, FunctionCalled}, Args}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  CallFunctionAtom = call_@,
   [UpdatedArgs | ArgsNumList] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, 'erlang'} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
        CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
        process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
        true ->
          NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleName} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
          CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
          process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]])
      end;
    false ->
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [ArgsNumListReturned | ArgsNumList])
  end;
process_arguments_call([{match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args}} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  CallFunctionAtom = call_@,
  [UpdatedArgs, ArgsNumList] = process_arguments_call(Args, Defs, FunctionName, ModuleName, [], []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [{atom, Num, ModuleName} | [{atom, Num, FunctionName} | [{atom, Num, ModuleCalled} | [{atom, Num, FunctionCalled} | [tuple_to_list(UpdatedArgs, Num)]]]]],
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, CallFunctionAtom}, NewArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), Pointcut} | [ArgsNumListReturned | ArgsNumList]]);
    false ->
      CallUpdated = {match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs}},
      process_function_call(T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm], [{length(UpdatedArgs), nil} | [ArgsNumListReturned | ArgsNumList]])
  end;
process_arguments_call([{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Args, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]);
process_arguments_call([{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  [UpdatedArgs, ArgsNumList] = process_case_call(Args, Defs, FunctionName, ModuleName, [], []),
  process_function_call(T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm], [ArgsNumList | ArgsNumListReturned]);
process_arguments_call([{_Type1, _Num1, {_Type2, _Num2, _, _Args}, _} = H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  process_arguments_call(T, Defs, FunctionName, ModuleName, [H | NewForm], ArgsNumListReturned);
process_arguments_call([{var, _, _} = H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  process_arguments_call(T, Defs, FunctionName, ModuleName, [H | NewForm], ArgsNumListReturned);
process_arguments_call([{_, _, _} = H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  process_arguments_call(T, Defs, FunctionName, ModuleName, [H | NewForm], ArgsNumListReturned);
process_arguments_call([H | T], Defs, FunctionName, ModuleName, NewForm, ArgsNumListReturned) ->
  process_arguments_call(T, Defs, FunctionName, ModuleName, [H | NewForm], ArgsNumListReturned).


check_inject_function_call([]) -> true;
check_inject_function_call([{function, _, FunctionName, NumArgs, _} | T]) ->
  Function = erlang:atom_to_list(FunctionName),
  if (Function == "call_@") and (NumArgs == 5) ->
    false;
    true ->
      check_inject_function_call(T)
  end;
check_inject_function_call([_ | T]) -> check_inject_function_call(T).

tuple_to_list([], Num) -> {nil, Num};
tuple_to_list([H], Num) -> {cons, Num, H, {nil, Num}};
tuple_to_list([H | T], Num) ->
  {cons, Num, H, tuple_to_list(T, Num)}.

generate_args(0, _, Result) -> lists:reverse(Result);
generate_args(Num, LineNum, Result) ->
  generate_args(Num - 1, LineNum, [{var, LineNum, list_to_atom("M" ++ integer_to_list(Num))} | Result]).


generate_match(0, LineNum) -> {nil, LineNum};
generate_match(1, LineNum) ->
  {cons, LineNum, {var, LineNum, 'M1'}, {nil, LineNum}};
generate_match(Num, LineNum) ->
  {cons, LineNum, {var, LineNum, list_to_atom("M" ++ integer_to_list(Num))}, generate_match(Num - 1, LineNum)}.


generate_case({Num, Pointcut}, LineNum) ->
  AdviceTypes = case Pointcut of
                  nil -> [];
                  _ -> Pointcut#pointcut.advices
                end,

  Return = {var, LineNum, 'R'},
  Call = case lists:any(fun(intercept) -> true; (_) -> false end, AdviceTypes) of
           true ->
             Intercept = {call, LineNum, {remote, LineNum, {atom, LineNum, advices}, {atom, LineNum, intercept_advice}}, [{atom, LineNum, call}, {call, LineNum, {atom, LineNum, self}, []}, {var, LineNum, 'Module'}, {var, LineNum, 'Function'}, {cons, LineNum, {var, LineNum, 'CalledModule'}, {cons, LineNum, {var, LineNum, 'CalledFunction'}, {cons, LineNum, {var, LineNum, 'Args'}, {nil, LineNum}}}}]},
             {match, LineNum, {var, LineNum, 'R'}, Intercept};
           false ->
             {match, LineNum, {var, LineNum, 'R'}, {call, LineNum, {remote, LineNum, {var, LineNum, 'CalledModule'}, {var, LineNum, 'CalledFunction'}}, generate_args(Num, LineNum, [])}}
         end,

  Before = {call, LineNum, {remote, LineNum, {atom, LineNum, advices}, {atom, LineNum, before_advice}}, [{atom, LineNum, call}, {call, LineNum, {atom, LineNum, self}, []}, {var, LineNum, 'Module'}, {var, LineNum, 'Function'}, {cons, LineNum, {var, LineNum, 'CalledModule'}, {cons, LineNum, {var, LineNum, 'CalledFunction'}, {cons, LineNum, {var, LineNum, 'Args'}, {nil, LineNum}}}}]},
  After = {call, LineNum, {remote, LineNum, {atom, LineNum, advices}, {atom, LineNum, after_advice}}, [{atom, LineNum, call}, {call, LineNum, {atom, LineNum, self}, []}, {var, LineNum, 'Module'}, {var, LineNum, 'Function'}, {cons, LineNum, {var, LineNum, 'CalledModule'}, {cons, LineNum, {var, LineNum, 'CalledFunction'}, {cons, LineNum, {var, LineNum, 'Args'}, {cons, LineNum, {var, LineNum, 'R'}, {nil, LineNum}}}}}]},
  Match = {match, LineNum, generate_match(Num, LineNum), {var, LineNum, 'Args'}},

  HasBefore = lists:any(fun(before) -> true; (_) -> false end, AdviceTypes),
  HasAfter = lists:any(fun('after') -> true; (_) -> false end, AdviceTypes),
  case HasBefore andalso HasAfter of
    true ->
      {clause, LineNum, [{integer, LineNum, Num}], [], [Before, Match, Call, After, Return]};
    false ->
      case HasBefore of
        true ->
          {clause, LineNum, [{integer, LineNum, Num}], [], [Before, Match, Call]};
        false ->
          case HasAfter of
            true -> {clause, LineNum, [{integer, LineNum, Num}], [], [Match, Call, After, Return]};
            false -> {clause, LineNum, [{integer, LineNum, Num}], [], [Call]}
          end
      end
  end.


generate_cases([], _, Result) -> lists:reverse(Result);
generate_cases([H | T], LineNum, Result) ->
  generate_cases(T, LineNum, [generate_case(H, LineNum) | Result]).

inject_function_call(LineNum, NumList) ->
  InterceptCall = call_@,
  FunctionName = {function, LineNum, InterceptCall, 5,
    [{clause, LineNum, [{var, LineNum, 'Module'}, {var, LineNum, 'Function'}, {var, LineNum, 'CalledModule'}, {var, LineNum, 'CalledFunction'}, {var, LineNum, 'Args'}], [],
      [{'case', LineNum, {call, LineNum, {atom, LineNum, length}, [{var, LineNum, 'Args'}]}, generate_cases(NumList, LineNum, [])}]}]},
  FunctionName.
