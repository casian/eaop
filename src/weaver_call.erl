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
  Aspects = filter(Defs, call, []),
  case Aspects of
    [] -> Forms;
    _ -> process_forms_call(0,Forms, Aspects, [], [])
  end.

filter([], _, Result) -> Result;
filter([H | T], Type, Result) ->
  #pointcut{event = Event} = H,
  if Event == Type -> filter(T, Type, [H | Result]);
    true -> filter(T, Type, Result)
  end.

is_match([], _, _, _, _) -> false;
is_match([Pointcut = #pointcut{event = call, module = Module, function = Function, arity = Arity, advice_types = _} | T], ModuleName, FunctionName, FunctionArity, Args) ->
  ModuleCheck = util:regex_match(atom_to_list(ModuleName), Module) or (Module == "_"),
  FunctionCheck = util:regex_match(atom_to_list(FunctionName), Function) or (Function == "_"),
  if is_atom(FunctionArity) == true ->
    ArityCheck = (Arity == "*") orelse util:regex_match(atom_to_list(FunctionArity), Arity),
    Check = ModuleCheck and FunctionCheck and ArityCheck,
    if Check == true ->
      {true, Pointcut};
      true ->
        is_match(T, ModuleName, FunctionName, FunctionArity, Args)
    end;
    is_integer(FunctionArity) == true ->
      ArityCheck = (Arity == "*") orelse util:regex_match(integer_to_list(FunctionArity), Arity),
      Check = ModuleCheck and FunctionCheck and ArityCheck,
      if Check == true ->
        {true, Pointcut};
        true ->
          is_match(T, ModuleName, FunctionName, FunctionArity, Args)
      end
  end.

instrument_args(Num, [ModuleName, FunctionName, ModuleCalled, FunctionCalled, ArgsList]) ->
  ArgsListSyntaxTree = list_to_parse_tree_list(ArgsList, Num),
  [{atom, Num, call}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, list_to_parse_tree_list([{atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}, ArgsListSyntaxTree], Num)].

instrument_call(Count,MatchedCall, AdviceTypes, Num, [ModuleName, FunctionName, ModuleCalled, FunctionCalled, ArgsList]) ->
  ArgsSyntax = instrument_args(Num, [ModuleName, FunctionName, ModuleCalled, FunctionCalled, ArgsList]),

  HasBefore = lists:any(fun(before) -> true; (_) -> false end, AdviceTypes),
  HasAfter = lists:any(fun('after') -> true; (_) -> false end, AdviceTypes),
  HasIntercept = lists:any(fun(intercept) -> true; (_) -> false end, AdviceTypes),

  Before = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, before_advice}}, ArgsSyntax},
  Intercept = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, intercept_advice}}, ArgsSyntax},

  RetVar = list_to_atom("RC"++integer_to_list(Count)),

  AfterArgsSyntax = instrument_args(Num, [ModuleName, FunctionName, ModuleCalled, FunctionCalled, ArgsList ++ [{var, Num, RetVar}]]),
  After = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, after_advice}}, AfterArgsSyntax},

  BL = if HasBefore == true -> [Before]; true -> [] end,
  IL = if HasIntercept == true -> lists:concat([[{match, Num, {var, Num, RetVar}, Intercept}], BL]);
         true -> lists:concat([[{match, Num, {var, Num, RetVar}, MatchedCall}], BL]) end,
  AL = if HasAfter == true -> lists:concat([[After], IL]); true -> IL end,
  AL.


process_forms_call(_,[], _, _, NewForm) -> lists:reverse(NewForm);
process_forms_call(Count,[{attribute, _, module, ModuleName} = H | T], Defs, _, NewForm) ->
  process_forms_call(Count,T, Defs, ModuleName, [H | NewForm]);
process_forms_call(Count,[{attribute, _, _, _} = H | T], Defs, ModuleName, NewForm) ->
  process_forms_call(Count,T, Defs, ModuleName, [H | NewForm]);
process_forms_call(Count,[{function, LineNum, FunctionName, NumArgs, Clause} | T], Defs, ModuleName, NewForm) ->
  if length(Clause) == 1 ->
    [{clause, LineNum, A, B, Contents}] = Clause,
    [Form] = process_function_call(Count,Contents, Defs, FunctionName, ModuleName, []),
    process_forms_call(Count,T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, [{clause, LineNum, A, B, Form}]} | NewForm]);
    length(Clause) > 1 ->
      [NewClause] = process_clauses_call(Count,Clause, Defs, FunctionName, ModuleName, []),
      process_forms_call(Count,T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, NewClause} | NewForm]);
    true -> {ok}
  end;
process_forms_call(Count,[{eof, C} | T], Defs, ModuleName, NewForm) ->
  process_forms_call(Count,T, Defs, ModuleName, [{eof, C} | NewForm]);
process_forms_call(Count,[_ | T], Defs, ModuleName, NewForm) ->
  process_forms_call(Count,T, Defs, ModuleName, NewForm).

process_clauses_call(_,[], _, _, _, NewClauses) ->
  [lists:reverse(NewClauses)];
process_clauses_call(Count,[Clause | T], Defs, FunctionName, ModuleName, NewClauses) ->
  {clause, LineNum, A, B, Contents} = Clause,
  [NewClause] = process_function_call(Count,Contents, Defs, FunctionName, ModuleName, []),
  process_clauses_call(Count,T, Defs, FunctionName, ModuleName, [{clause, LineNum, A, B, NewClause} | NewClauses]).

process_function_call(_,[], _, _, _, NewForm) -> [lists:reverse(NewForm)];
process_function_call(Count,[{call, Num, {atom, Num, FunctionCalled}, Args} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [ModuleName, FunctionName, 'erlang', FunctionCalled, UpdatedArgs],
        CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
        process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
        true ->
          NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
          CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
          process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]))
      end;
    false ->
      CallUpdated = {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_function_call(Count,[{call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [ModuleName, FunctionName, ModuleCalled, FunctionCalled, UpdatedArgs],
      CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
      process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
    false ->
      CallUpdated = {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_function_call(Count,[{match, Num, A, {call, Num, {atom, Num, FunctionCalled}, Args}} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
        CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
        process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
        true ->
          NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
          CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
          process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]))
      end;
    false ->
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs}},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_function_call(Count,[{match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args}} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [ModuleName, FunctionName, ModuleCalled, FunctionCalled, UpdatedArgs],
      CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
      process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
    false ->
      CallUpdated = {match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs}},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]))
  end;

process_function_call(Count,[{match, Num2, R, {'receive', Num, Clauses}} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Clauses, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs}} | NewForm]);

process_function_call(Count,[{match, Num2, R, {'receive', Num, Clauses, OP, Clauses2}} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Clauses, Defs, FunctionName, ModuleName, []),
  [UpdatedArgs2] = process_case_call(Count,Clauses2, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs, OP, UpdatedArgs2}} | NewForm]);

process_function_call(Count,[{'receive', Num, Clauses} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Clauses, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs} | NewForm]);

process_function_call(Count,[{'receive', Num, Clauses, OP, Clauses2} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Clauses, Defs, FunctionName, ModuleName, []),
  [UpdatedArgs2] = process_case_call(Count,Clauses2, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs, OP, UpdatedArgs2} | NewForm]);

process_function_call(Count,[{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm]);
process_function_call(Count,[{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_if_call(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm]);
process_function_call(Count,[H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]).

process_case_call(_,[], _, _, _, NewForm) -> [lists:reverse(NewForm)];
process_case_call(Count,[H | T], Defs, FunctionName, ModuleName, NewForm) ->
  {clause, A, B, C, Arg} = H,
  [UpdatedArgs] = process_arguments_call(Count,Arg, Defs, FunctionName, ModuleName, []),
  process_case_call(Count,T, Defs, FunctionName, ModuleName, [{clause, A, B, C, UpdatedArgs} | NewForm]).

process_if_call(_,[], _, _, _, NewForm) -> [lists:reverse(NewForm)];
process_if_call(Count,[H | T], Defs, FunctionName, ModuleName, NewForm) ->
  {clause, A, B, C, Arg} = H,
  [UpdatedArgs] = process_arguments_call(Count,Arg, Defs, FunctionName, ModuleName, []),
  process_if_call(Count,T, Defs, FunctionName, ModuleName, [{clause, A, B, C, UpdatedArgs} | NewForm]).


process_arguments_call(_,[], _, _, _, NewForm) -> [lists:reverse(NewForm)];
process_arguments_call(Count,[{call, Num, {atom, Num, FunctionCalled}, Args} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [ModuleName, FunctionName, 'erlang', FunctionCalled, UpdatedArgs],
        CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
        process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
        true ->
          NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
          CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
          process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]))
      end;
    false ->
      CallUpdated = {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_arguments_call(Count,[{call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [ModuleName, FunctionName, ModuleCalled, FunctionCalled, UpdatedArgs],
      CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
      process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
    false ->
      CallUpdated = {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs},
      process_function_call(Count+1,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_arguments_call(Count,[{match, Num, A, {call, Num, {atom, Num, FunctionCalled}, Args}} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      Check = lists:member(FunctionCalled, ?BIFS),
      if Check == true ->
        NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
        CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
        process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
        true ->
          NewArgs = [ModuleName, FunctionName, ModuleName, FunctionCalled, UpdatedArgs],
          CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
          process_function_call(Count+1,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
      end;
    false ->
      CallUpdated = {match, Num, A, {call, Num, {atom, Num, FunctionCalled}, UpdatedArgs}},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_arguments_call(Count,[{match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, Args}} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_arguments_call(Count,Args, Defs, FunctionName, ModuleName, []),
  Match = is_match(Defs, ModuleName, FunctionCalled, length(Args), Args),
  case Match of
    {true, Pointcut} ->
      NewArgs = [ModuleName, FunctionName, ModuleCalled, FunctionCalled, UpdatedArgs],
      CallUpdated = instrument_call(Count,H, Pointcut#pointcut.advice_types, Num, NewArgs),
      process_function_call(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([CallUpdated, NewForm]));
    false ->
      CallUpdated = {match, Num, A, {call, Num, {remote, Num, {atom, Num, ModuleCalled}, {atom, Num, FunctionCalled}}, UpdatedArgs}},
      process_function_call(Count,T, Defs, FunctionName, ModuleName, [CallUpdated | NewForm])
  end;
process_arguments_call(Count,[{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm]);
process_arguments_call(Count,[{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  [UpdatedArgs] = process_case_call(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_call(Count,T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm]);
process_arguments_call(Count,[{_Type1, _Num1, {_Type2, _Num2, _, _Args}, _} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_arguments_call(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]);
process_arguments_call(Count,[{var, _, _} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_arguments_call(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]);
process_arguments_call(Count,[{_, _, _} = H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_arguments_call(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]);
process_arguments_call(Count,[H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_arguments_call(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]).

list_to_parse_tree_list([], Num) ->
  {nil, Num};
list_to_parse_tree_list([H], Num) ->
  {cons, Num, H, {nil, Num}};
list_to_parse_tree_list([H | T], Num) ->
  {cons, Num, H, list_to_parse_tree_list(T, Num)}.

