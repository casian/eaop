-module(weaver_receive).

-include("aop.hrl").

-export([handle_receive/2]).

-import(proplists, []).
-import(re, []).
-import(lists, []).
-import(string, []).
-import(fun_proxy, []).


handle_receive(Forms, Defs) ->
  Aspects = filter(Defs, 'receive', []),
  NewForm = process_forms_receive(0,Forms, Aspects, [], []),
  NewForm.

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

process_forms_receive(_,[], _, _, NewForm) -> lists:reverse(NewForm);
process_forms_receive(Count,[{attribute, _, module, ModuleName} = H | T], Defs, _, NewForm) ->
  process_forms_receive(Count,T, Defs, ModuleName, [H | NewForm]);
process_forms_receive(Count,[{attribute, _, _, _} = H | T], Defs, ModuleName, NewForm) ->
  process_forms_receive(Count,T, Defs, ModuleName, [H | NewForm]);
process_forms_receive(Count,[{function, LineNum, FunctionName, NumArgs, Clause} | T], Defs, ModuleName, NewForm) ->
  if length(Clause) == 1 ->
    [{clause, LineNum, A, B, Contents}] = Clause,
    Form = process_function_receive(Count,Contents, Defs, FunctionName, ModuleName, []),
    process_forms_receive(Count,T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, [{clause, LineNum, A, B, Form}]} | NewForm]);
    length(Clause) > 1 ->
      NewClause = process_clauses_receive(Count,Clause, Defs, FunctionName, ModuleName, []),
      process_forms_receive(Count,T, Defs, ModuleName, [{function, LineNum, FunctionName, NumArgs, NewClause} | NewForm]);
    true -> {ok}
  end;
process_forms_receive(Count,[{eof, C} | T], Defs, ModuleName, NewForm) ->
  process_forms_receive(Count,T, Defs, ModuleName, [{eof, C} | NewForm]);
process_forms_receive(Count,[_ | T], Defs, ModuleName, NewForm) ->
  process_forms_receive(Count,T, Defs, ModuleName, NewForm).

process_clauses_receive(_,[], _, _, _, NewClauses) -> lists:reverse(NewClauses);
process_clauses_receive(Count,[Clause | T], Defs, FunctionName, ModuleName, NewClauses) ->
  {clause, LineNum, A, B, Contents} = Clause,
  NewClause = process_function_receive(Count,Contents, Defs, FunctionName, ModuleName, []),
  process_clauses_receive(Count,T, Defs, FunctionName, ModuleName, [{clause, LineNum, A, B, NewClause} | NewClauses]).

receive_matches([], _) -> false;
receive_matches([{clause, _, Msg, _, _} | T], Defs) ->
  case lists:any(fun(Def) -> Pattern = Def#pointcut.payload,
    str_pattern_match(Pattern, weaver:msg_to_string(Msg)) end, Defs) of
    true -> true;
    false -> receive_matches(T, Defs)
  end.

process_receive(Count,RecvWrapperFun, MatchWrapperFun, Clauses, Num, Defs, FunctionName, ModuleName) ->
  Match = is_match(Defs, ModuleName, FunctionName),
  ValidReceive = receive_matches(Clauses, Defs),
  case Match of
    {true, Pointcut} ->
      if (ValidReceive == true) ->
        ReceiveBefore = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, before_advice}}, [{atom, Num, 'receive'}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {nil, Num}]},
        ReceiveContents = RecvWrapperFun(Num, process_contents_receive(Clauses, Defs, FunctionName, ModuleName, [])),

        AdviceTypes = Pointcut#pointcut.advice_types,
        HasBefore = lists:member(before, AdviceTypes),
        HasAfter = lists:member('after', AdviceTypes),
        HasIntercept = lists:member(intercept, AdviceTypes),
        HasUpon = lists:member(upon, AdviceTypes),


        BL = if HasBefore == true -> [ReceiveBefore]; true -> [] end,
        UL = if HasUpon == true -> lists:concat([[ReceiveContents], BL]); true ->
          lists:concat([[RecvWrapperFun(Num, Clauses)], BL]) end,

        [Recv | Rest] = UL,
        IL = if HasIntercept == true ->
          Intercept = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, intercept_advice}}, [{atom, Num, 'receive'}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {cons, Num, {'fun', Num, {clauses, [{clause, Num, [], [], [Recv]}]}}, {nil, Num}}]},
          lists:concat([[MatchWrapperFun(Intercept)], Rest]);
               true -> [MatchWrapperFun(Recv) | Rest]
             end,

        RetVar = list_to_atom("RR"++integer_to_list(Count)),

        AL = if HasAfter == true ->
          [RL | T] = IL,
          MRL = {match, Num, {var, Num, RetVar}, RL},
          ReceiveAfter = {call, Num, {remote, Num, {atom, Num, advices}, {atom, Num, after_advice}}, [{atom, Num, 'receive'}, {call, Num, {atom, Num, self}, []}, {atom, Num, ModuleName}, {atom, Num, FunctionName}, {var, Num, RetVar}]},
          lists:concat([[ReceiveAfter, MRL], T]);
               true -> IL
             end,
        AL;
        true ->
          [MatchWrapperFun(RecvWrapperFun(Num, Clauses))]
      end;
    false ->
      [MatchWrapperFun(RecvWrapperFun(Num, Clauses))]
  end.

process_function_receive(_,[], _, _, _, NewForm) -> lists:reverse(NewForm);

process_function_receive(Count,[{match, Num2, R, {'receive', Num, Clauses}} | T], Defs, FunctionName, ModuleName, NewForm) ->
  MatchWrapperFun = fun(Recv) -> {match, Num2, R, Recv} end,
  RecvWrapperFun = fun(Num, Clauses) -> {'receive', Num, Clauses} end,
  Res = process_receive(Count,RecvWrapperFun, MatchWrapperFun, Clauses, Num, Defs, FunctionName, ModuleName),
  process_function_receive(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([Res, NewForm]));

process_function_receive(Count,[{match, Num2, R, {'receive', Num, Clauses, OP, Clauses2}} | T], Defs, FunctionName, ModuleName, NewForm) ->
  MatchWrapperFun = fun(Recv) -> {match, Num2, R, Recv} end,
  RecvWrapperFun = fun(Num, Clauses) -> {'receive', Num, Clauses, OP, Clauses2} end,
  Res = process_receive(Count,RecvWrapperFun, MatchWrapperFun, Clauses, Num, Defs, FunctionName, ModuleName),
  process_function_receive(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([Res, NewForm]));

process_function_receive(Count,[{'receive', Num, Clauses} | T], Defs, FunctionName, ModuleName, NewForm) ->
  MatchWrapperFun = fun(Recv) -> Recv end,
  RecvWrapperFun = fun(Num, Clauses) -> {'receive', Num, Clauses} end,
  Res = process_receive(Count,RecvWrapperFun, MatchWrapperFun, Clauses, Num, Defs, FunctionName, ModuleName),
  process_function_receive(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([Res, NewForm]));

process_function_receive(Count,[{'receive', Num, Clauses, OP, Clauses2} | T], Defs, FunctionName, ModuleName, NewForm) ->
  MatchWrapperFun = fun(Recv) -> Recv end,
  RecvWrapperFun = fun(Num, Clauses) -> {'receive', Num, Clauses, OP, Clauses2} end,
  Res = process_receive(Count,RecvWrapperFun, MatchWrapperFun, Clauses, Num, Defs, FunctionName, ModuleName),
  process_function_receive(Count+1,T, Defs, FunctionName, ModuleName, lists:concat([Res, NewForm]));


process_function_receive(Count,[{'case', Num, A, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_receive(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_receive(Count,T, Defs, FunctionName, ModuleName, [{'case', Num, A, UpdatedArgs} | NewForm]);
process_function_receive(Count,[{'if', Num, Args} = _H | T], Defs, FunctionName, ModuleName, NewForm) ->
  UpdatedArgs = process_function_receive(Count,Args, Defs, FunctionName, ModuleName, []),
  process_function_receive(Count,T, Defs, FunctionName, ModuleName, [{'if', Num, UpdatedArgs} | NewForm]);
process_function_receive(Count,[H | T], Defs, FunctionName, ModuleName, NewForm) ->
  process_function_receive(Count,T, Defs, FunctionName, ModuleName, [H | NewForm]).


str_pattern_match(PointCutPattern, Pattern) ->
  RePointCutPattern = re:replace(PointCutPattern, "_", "(\\\\{|\\\\[)?[A-Za-z0-9,_\\\\[\\\\]\\\\{\\\\}]*(\\\\}|\\\\])?", [global, {return, list}]),
  IsMatch = re:run(Pattern, RePointCutPattern),
  case IsMatch of
    nomatch -> false;
    {match, _} -> true
  end.

process_contents_receive([], _, _, _, NewForm) -> lists:reverse(NewForm);
process_contents_receive([{clause, Num, Msg, A, Contents} | T], Defs, FunctionName, ModuleName, NewForm) ->
  InnerMsg =
    case Msg of
      [{match,_,_,Tuple}] -> Tuple;
      [Tuple] -> Tuple
    end,
  case lists:any(fun(Def) -> Pattern = Def#pointcut.payload,
    str_pattern_match(Pattern, weaver:msg_to_string([InnerMsg])) end, Defs) of
    %%case true of
    true ->
      if erlang:element(3, InnerMsg) == '_' ->
        ReceiveUpon = {call, Num + 1, {remote, Num + 1, {atom, Num + 1, advices}, {atom, Num + 1, upon_advice}}, [{atom, Num + 1, 'receive'}, {call, Num + 1, {atom, Num + 1, self}, []}, {atom, Num + 1, ModuleName}, {atom, Num + 1, FunctionName}, {nil, Num + 1}]},
        UpdatedClause = {clause, Num, Msg, A, [ReceiveUpon | Contents]},
        process_contents_receive(T, Defs, FunctionName, ModuleName, [UpdatedClause | NewForm]);
        true ->
          ReceiveUpon = {call, Num + 1, {remote, Num + 1, {atom, Num + 1, advices}, {atom, Num + 1, upon_advice}}, [{atom, Num + 1, 'receive'}, {call, Num + 1, {atom, Num + 1, self}, []}, {atom, Num + 1, ModuleName}, {atom, Num + 1, FunctionName}, InnerMsg]},
          UpdatedClause = {clause, Num, Msg, A, [ReceiveUpon | Contents]},
          process_contents_receive(T, Defs, FunctionName, ModuleName, [UpdatedClause | NewForm])
      end;
    false ->
      process_contents_receive(T, Defs, FunctionName, ModuleName, [{clause, Num, Msg, A, Contents} | NewForm])
  end;
process_contents_receive([_ | T], Defs, FunctionName, ModuleName, Form) ->
  process_contents_receive(T, Defs, FunctionName, ModuleName, Form).
