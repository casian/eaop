-module(weaver_override).
-export([handle_override/2]).
-include("aop.hrl").


filter([], _, Result) -> Result;
filter([H | T], Type, Result) ->
  #pointcut{event = Event} = H,
  if Event == Type -> filter(T, Type, [H | Result]);
    true -> filter(T, Type, Result)
  end.

handle_override(Forms, Defs) ->
  Aspects = filter(Defs, function, []),
  {ExportedFuncs, NewForms} = process_forms_function(Forms, Aspects, [], {[], []}, []),
  export_overridden_functions(NewForms, {ExportedFuncs, []}).

encode_intercepted_function_name(FunctionName) ->
  ListFunctionName =
    if is_list(FunctionName) == true ->
      FunctionName;
      is_atom(FunctionName) == true ->
        atom_to_list(FunctionName)
    end,
  list_to_atom(ListFunctionName ++ "_@").

process_forms_function([], _, _, {ExportedFuncs, NewForm}, _) ->
  {ExportedFuncs, lists:reverse(NewForm)};
process_forms_function([{attribute, _, module, ModuleName} = H | T], Defs, _, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
  process_forms_function(T, Defs, ModuleName, {ExportedFuncs, [H | NewForm]}, InterceptedFunctions);

%%process_forms_function([{attribute, LineNum, export, ExportList} | T], Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
%%  process_forms_function(T, Defs, ModuleName, {ExportedFuncs,[{attribute, LineNum, export, ExportList} | NewForm]}, InterceptedFunctions);

process_forms_function([{attribute, _, _, _} = H | T], Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
  process_forms_function(T, Defs, ModuleName, {ExportedFuncs, [H | NewForm]}, InterceptedFunctions);
process_forms_function([{function, LineNum, FunctionName, NumArgs, Clause} = H | T], Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
  case lists:any(
    fun(Def) ->
      ModulePattern = string:strip(Def#pointcut.module),
      ModuleCheck =  util:regex_match(atom_to_list(ModuleName),ModulePattern) orelse (ModulePattern == "_"),
      ArityPattern = string:strip(Def#pointcut.arity),
      ArityCheck = (ArityPattern == "*") orelse util:regex_match(integer_to_list(NumArgs), ArityPattern),
      FunctionPattern = string:strip(Def#pointcut.function),
      FunctionCheck = util:regex_match(atom_to_list(FunctionName),FunctionPattern) orelse (FunctionPattern == "_"),
      ModuleCheck andalso ArityCheck andalso FunctionCheck end, Defs
    )
  of
    true ->
      NewFunctionName = encode_intercepted_function_name(FunctionName),
      process_forms_function(T, Defs, ModuleName, {ExportedFuncs, [{function, LineNum, NewFunctionName, NumArgs, Clause} | NewForm]}, InterceptedFunctions ++ [{function, LineNum, FunctionName, NumArgs, Clause}]);
    false -> process_forms_function(T, Defs, ModuleName, {ExportedFuncs, [H | NewForm]}, InterceptedFunctions)
  end;
process_forms_function([{eof, C} | T], Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
  {NewExportedFuncs, InjectedFunctionList} = inject_function_function(ModuleName, C, InterceptedFunctions, {ExportedFuncs, NewForm}),
  process_forms_function(T, Defs, ModuleName, {NewExportedFuncs, [{eof, C + 5} | InjectedFunctionList]}, InterceptedFunctions);
process_forms_function([_ | T], Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions) ->
  process_forms_function(T, Defs, ModuleName, {ExportedFuncs, NewForm}, InterceptedFunctions).


inject_function_function(_, _, [], {ExportedFuncs, NewForm}) -> {ExportedFuncs, NewForm};
inject_function_function(ModuleName, LineNum, [{function, _, FunctionName, NumArgs, [Clause]} | InterceptedFunctions], {ExportedFuncs, NewForm}) ->
  InterceptedClause = process_clause_function(Clause, LineNum, ModuleName, FunctionName),
  InjectedFunction = {function, LineNum, FunctionName, NumArgs, [InterceptedClause]},
  {NewExportedFuncs, InjectedFunctionList} = inject_function_function(ModuleName, LineNum, InterceptedFunctions, {ExportedFuncs, NewForm}),
  {[{FunctionName, NumArgs} | NewExportedFuncs], [InjectedFunction | InjectedFunctionList]}.


export_overridden_functions([], {_, NewForm}) -> lists:reverse(NewForm);
export_overridden_functions([{attribute, LineNum, export, ExportList} | T], {ExportedFuncs, NewForm}) ->
  NewExportList = ExportList ++ lists:map(fun({Func, Arity}) ->
    {encode_intercepted_function_name(Func), Arity} end, ExportedFuncs),
  export_overridden_functions(T, {ExportedFuncs, [{attribute, LineNum, export, NewExportList} | NewForm]});
export_overridden_functions([H | T], {ExportedFuncs, NewForm}) ->
  export_overridden_functions(T, {ExportedFuncs, [H | NewForm]}).



args_to_list([], LineNum) -> {nil, LineNum};
args_to_list([H | T], LineNum) -> {cons, LineNum, H, args_to_list(T, LineNum)}.

process_clause_function({clause, _, ArgsList, _, _}, LineNum, ModuleName, FunctionName) ->
  NewArgsList = lists:map(fun({var, _, VarName}) -> {var, LineNum, VarName} end, ArgsList),
  Body = [{call, LineNum, {remote, LineNum, {atom, LineNum, advices}, {atom, LineNum, override_advice}}, [
    {atom, LineNum, function}, %%Type
    {call, LineNum, {atom, LineNum, self}, []}, %%PID
    {atom, LineNum, ModuleName}, %% MODULE
    {atom, LineNum, FunctionName}, %% FUNCTION
    %{tuple, LineNum, [{atom, LineNum, ModuleName}, {atom, LineNum, encode_intercepted_function_name(FunctionName)}, args_to_list(NewArgsList, LineNum)]}
    {cons, LineNum, {atom, LineNum, ModuleName}, {cons, LineNum, {atom, LineNum, encode_intercepted_function_name(FunctionName)}, {cons, LineNum, args_to_list(NewArgsList, LineNum), {nil, LineNum}}}}
  ]}],


  %Body = {atom, LineNum, ok},
  {clause, LineNum, NewArgsList, [], Body}.