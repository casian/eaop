-module(weaver_send).

-include("aop.hrl").

-export([handle_send/2]).

-import(proplists, []).
-import(re, []).
-import(lists, []).
-import(string, []).
-import(fun_proxy, []).

handle_send(Forms, Defs) ->
  Aspects = filter(Defs, send, []),
  process_forms_send(Forms, Aspects, [], []).

filter([], _, Result) -> Result;
filter([H | T], Type, Result) ->
  #pointcut{event = Event} = H,
  if Event == Type -> filter(T, Type, [H | Result]);
    true -> filter(T, Type, Result)
  end.


is_match([], _, _) -> false;
is_match([H | T], ModuleName, FunctionName) ->
  #pointcut{event = _Event, module = Module, function = Function, payload = _Arity} = H,
  ModuleCheck = util:regex_match(atom_to_list(ModuleName), Module) or (Module == "_"),
  FunctionCheck = util:regex_match(atom_to_list(FunctionName),Function) or (Function == "_"),
  Check = ModuleCheck and FunctionCheck,
  if Check == true ->
    {true, H};
    true ->
      is_match(T, ModuleName, FunctionName)
  end.

process_forms_send([], _, _, NewForm) -> lists:reverse(NewForm);
process_forms_send([{attribute, _, module, ModuleName} = H | T], Defs, _, NewForm) ->
  process_forms_send(T, Defs, ModuleName, [H | NewForm]);
process_forms_send([{attribute, _, _, _} = H | T], Defs, ModuleName, NewForm) ->
  process_forms_send(T, Defs, ModuleName, [H | NewForm]);
process_forms_send([{function, LineNum, FunctionName, NumArgs, Clause} | T], Defs, ModuleName, NewForm) ->
  if length(Clause) == 1 ->
    [{clause, LineNum, A, B, Contents}] = Clause,
    Form = process_function_send(Contents, Defs, FunctionName, ModuleName, []),
    process_forms_send(T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, [{clause, LineNum, A, B, Form}]} | NewForm]);
    length(Clause) > 1 ->
      NewClause = process_clauses_send(Clause, Defs, FunctionName, ModuleName, []),
      process_forms_send(T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, NewClause} | NewForm]);
    true -> {ok}
  end;
process_forms_send([{eof, C} | T], Defs, ModuleName, NewForm) ->
  %Check = check_inject_function_send(NewForm),
  %if Check == true ->
  %  process_forms_send(T, Defs, ModuleName, [{eof, C + 5} | [inject_function_send(C) | NewForm]]);
  %  true ->
  process_forms_send(T, Defs, ModuleName, [{eof, C + 1} | NewForm]);
% end;
process_forms_send([_ | T], Defs, ModuleName, NewForm) ->
  process_forms_send(T, Defs, ModuleName, NewForm).

process_clauses_send([], _, _, _, NewClauses) -> lists:reverse(NewClauses);
process_clauses_send([Clause | T], Defs, FunctionName, ModuleName, NewClauses) ->
  {clause, LineNum, A, B, Contents} = Clause,
  NewClause = process_function_send(Contents, Defs, FunctionName, ModuleName, []),
  process_clauses_send(T, Defs, FunctionName, ModuleName, [{clause, LineNum, A, B, NewClause} | NewClauses]).

str_pattern_match(PointCutPattern, Pattern) ->
  RePointCutPattern = re:replace(PointCutPattern, "_", "(\\\\{|\\\\[)?[A-Za-z0-9,_\\\\[\\\\]\\\\{\\\\}]*(\\\\}|\\\\])?", [global, {return, list}]),
  IsMatch = re:run(Pattern, RePointCutPattern),
  case IsMatch of
    nomatch -> false;
    {match, _} -> true
  end.

process_send(H = {op, Num, '!', {T1, Num, SendTo}, Msg}, Defs, FunctionName, ModuleName) ->
  Match = is_match(Defs, ModuleName, FunctionName),
  PointCutPattern = weaver:msg_to_string(Msg),
  case lists:any(fun(Def) -> Pattern = Def#pointcut.payload, str_pattern_match(PointCutPattern, Pattern) end, Defs) of
    true ->
      case Match of
        {true, Pointcut} ->
          AdviceTypes = Pointcut#pointcut.advice_types,
          HasBefore = lists:any(fun(before) -> true; (_) -> false end, AdviceTypes),
          HasAfter = lists:any(fun('after') -> true; (_) -> false end, AdviceTypes),
          HasIntercept = lists:any(fun(intercept) -> true; (_) -> false end, AdviceTypes),

          %SendUpdated = {call, Num, {atom, Num, send_@}, [{atom, Num, ModuleName}, {atom, Num, FunctionName}, {T1, Num, SendTo}, Msg]},
          Before = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, before_advice}}, [{atom, Num, send}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {cons, Num, {T1, Num, SendTo}, {cons, Num, Msg, {nil, Num}}}]},
          After = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, after_advice}}, [{atom, Num, send}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {cons, Num, {T1, Num, SendTo}, {cons, Num, Msg, {nil, Num}}}]},
          Intercept = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, intercept_advice}}, [{atom, Num, send}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {cons, Num, {T1, Num, SendTo}, {cons, Num, Msg, {nil, Num}}}]},

          BL = if HasBefore == true -> [Before]; true -> [] end,
          IL = if HasIntercept == true -> lists:concat([[Intercept], BL]); true -> lists:concat([[H], BL]) end,
          AL = if HasAfter == true -> lists:concat([[After], IL]); true -> IL end,
          lists:concat([[Msg], AL]);
        false ->
          [H]
      end;
    false -> [H]
  end.

process_function_send([], _, _, _, NewForm) -> lists:reverse(NewForm);
process_function_send([{match, Num2, R, {op, Num, '!', {T1, Num, SendTo}, Msg}} | T], Defs, FunctionName, ModuleName, NewForm) ->
  [H | Res] = process_send({op, Num, '!', {T1, Num, SendTo}, Msg}, Defs, FunctionName, ModuleName),
  process_function_send(T, Defs, FunctionName, ModuleName, lists:concat([[{match, Num2, R, H}], Res, NewForm]));

process_function_send([{op, Num, '!', {T1, Num, SendTo}, Msg} | T], Defs, FunctionName, ModuleName, NewForm) ->
  Res = process_send({op, Num, '!', {T1, Num, SendTo}, Msg}, Defs, FunctionName, ModuleName),
  process_function_send(T, Defs, FunctionName, ModuleName, lists:concat([Res, NewForm]));

process_function_send([{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm]);
process_function_send([{'receive', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs} | NewForm]);

process_function_send([{'receive', Num, Args,Op,Clause2} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{'receive', Num, UpdatedArgs,Op,Clause2} | NewForm]);

process_function_send([{match, Num2, R, {'receive', Num, Args}} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs}} | NewForm]);

process_function_send([{match, Num2, R, {'receive', Num, Args,Op,Clause2}} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{match, Num2, R, {'receive', Num, UpdatedArgs,Op,Clause2}} | NewForm]);

process_function_send([{clause, Num, D, E, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{clause, Num, D, E, UpdatedArgs} | NewForm]);
process_function_send([{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_send(Args, Defs, FunctionName, ModuleName, []),
  process_function_send(T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm]);
process_function_send([H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_function_send(T, Defs, FunctionName, ModuleName, [H | NewForm]).
