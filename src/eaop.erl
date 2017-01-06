-module(eaop).
-author("Ian Cassar").

%%
%% Import modules
%%
-import(compile, []).
-import(filelib, []).
-import(file, []).
-import(lists, []).

%%
%% Exported Functions
%%
-export([instrument/2, instrument/3, instrument_srcs/3, instrument_beams/3]).

%%
%% API Functions
%%

%% @spec compile(Src_dirs::list() , Config_dirs::list()) -> ok
%% @throws nothing
%% @doc 
%%	Src_dirs = list(string()) - list of directory names these contain source files,
%%	Config_dirs = list(string()) - list of directory names these contain configuration files.
%% The function compiles a set of source files (*.erl) from source directories and applyes
%% AOP weaving during compilation stage. Configuration files for AOP weaving are taken
%% from configuration directories (*.eaop)
%%
instrument(Src_dirs, Config_dirs) ->
  instrument(Src_dirs, Config_dirs, []).

%% @spec compile(Src_dirs::list(), Config_dirs::list(), Options::list()) -> ok
%% @throws nothing
%% @doc 
%%	Src_dirs = list(string()) - list of directory names these contain source files,
%%	Config_dirs = list(string()) - list of directory names these contain configuration files,
%%	Options = list() - Options determine the behavior of the compiler as defined in compile:file/2.
%% The function compiles a set of source files (*.erl) from source directories and applyes
%% AOP weaving during compilation stage. Configuration files for AOP weaving are taken
%% from configuration directories (*.eaop)
%%
instrument([], _, _) -> ok;
instrument(Code_dirs, Config_dirs, Options) ->
  Src_File_list = get_source_files(Code_dirs, []),
  Beam_File_list = get_beam_files(Code_dirs, []),
  %%io:format("Sources = ~p ~n", [File_list]),
  Config = get_configurations(Config_dirs, [], Options),
  Opts = lists:append(
    [{parse_transform, weaver}, return, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(BeamFile) -> compile_beam_file(BeamFile, Opts) end, Beam_File_list),
  lists:foreach(fun(SrcFile) -> compile_src_file(SrcFile, Opts) end, Src_File_list),
  print_advice_template(Options),
  compile_advices(Options).

write_to_file(ModuleName, Bin, OutDir) ->
  write_to_file(ModuleName, Bin, OutDir, "beam").
write_to_file(ModuleName, Bin, OutDir, Ext) ->
  FileName = io_lib:format("~s\\~p.~s", [OutDir, ModuleName, Ext]),
  file:write_file(FileName, Bin, [write]).

compile_beam_file(BeamFile, Options) ->
  OutDir = case lists:filter(fun(Opt) -> case Opt of {outdir, _} -> true; _ -> false end end, Options) of
             [{outdir, Dir}] -> Dir;
             [] -> {ok, Dir} = file:get_cwd(), Dir
           end,
  {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(BeamFile, [abstract_code]),
  case compile:forms(AC, Options) of
    {ok, ModuleName, Bin} ->
      write_to_file(ModuleName, Bin, OutDir),
      util:print_msg("~n>> Module ~p was instrumented.", [ModuleName], Options),
      {ok, ModuleName};
    {ok, ModuleName, Bin, Warnings} ->
      lists:foreach(fun(Warning) -> util:print_warning("~p", [Warning], Options) end, Warnings),
      write_to_file(ModuleName, Bin, OutDir),
      util:print_msg("~n>> Module ~p was instrumented with Warnings. ~n", [ModuleName], Options),
      {ok, ModuleName};
    error -> util:print_error("Unexpected Error, STACK TRACE: ~p", [erlang:get_stacktrace()], Options);
    {error, Errors, Warnings} ->
      lists:foreach(fun(Warning) -> util:print_warning("~p", [Warning], Options) end, Warnings),
      lists:foreach(fun(Error) -> util:print_error("~p", [Error], Options) end, Errors),
      {error, Errors, Warnings}
  end.

compile_src_file(File, Options) ->
  case compile:file(File, Options) of
    {ok, ModuleName} ->
      util:print_msg("~n>> Module ~p was instrumented.", [ModuleName], Options),
      {ok, ModuleName};
    {ok, ModuleName, Warnings} ->
      lists:foreach(fun(Warning) -> util:print_warning("~p", [Warning], Options) end, Warnings),
      util:print_msg("~n>> Module ~p was instrumented with Warnings. ~n", [ModuleName], Options),
      {ok, ModuleName};
    error -> util:print_error("Unexpected Error, STACK TRACE: ~p", [erlang:get_stacktrace()], Options);
    {error, Errors, Warnings} ->
      lists:foreach(fun(Warning) -> util:print_warning("~p", [Warning], Options) end, Warnings),
      lists:foreach(fun(Error) -> util:print_error("~p", [Error], Options) end, Errors),
      {error, Errors, Warnings}
  end.

instrument_srcs([], _, _) -> ok;
instrument_srcs(File_list, Config_dirs, Options) ->
  %%io:format("Sources = ~p ~n", [File_list]),
  Config = get_configurations(Config_dirs, [], Options),
  Opts = lists:append(
    [{parse_transform, weaver}, return, report, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(SrcFile) -> compile_src_file(SrcFile, Opts) end, File_list),
  print_advice_template(Options),
  compile_advices(Options).


instrument_beams([], _, _) -> ok;
instrument_beams(File_list, Config_dirs, Options) ->
  %%io:format("Sources = ~p ~n", [File_list]),
  Config = get_configurations(Config_dirs, [], Options),
  Opts = lists:append(
    [{parse_transform, weaver}, return, report, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(BeamFile) -> compile_beam_file(BeamFile, Opts) end, File_list),
  print_advice_template(Options),
  compile_advices(Options).


get_files(_, [], Result) -> Result;
get_files(Ext, [Dir | Dirs], Result) ->
%	io:format(">>> get_source_files ~p~n", [Dir]),
  File_names = filelib:fold_files(Dir, Ext, true, fun(F, Acc) -> [F | Acc] end, []),
  get_source_files(Dirs, lists:append(File_names, Result)).

get_source_files(Dirs, Result) ->
  get_files(".*\.erl$", Dirs, Result).

get_beam_files(Dirs, Result) ->
  get_files(".*\.beam$", Dirs, Result).


%% @spec get_configurations(Dirs::list(), Result::list()) -> list(#aspect{})
%% @throws nothing
%% @doc Gets configuration files from list of directories and merges it to one
%% config list.
get_configurations([], Result, Options) ->
  case poincut_type_check(Result,false,Options) of
    false ->  Result;
    true ->  error(invalid_pointcuts) end;
get_configurations([Dir | Dirs], Result, Options) ->
  EAOP_files = filelib:fold_files(Dir, ".*\.eaop$", true, fun(F, Acc) -> [F | Acc] end, []),
  util:print_msg("Config files  = ~p~n", [EAOP_files], Options),
  get_configurations(Dirs, lists:append(read_eaop(EAOP_files, Options), Result), Options).

int_to_ordinal(Num) ->
  StrInt = integer_to_list(Num),
  LastNum = string:sub_string(StrInt,string:len(StrInt),string:len(StrInt)),
  case LastNum of
    "1" -> util:format("~pst",[Num]);
    "2" -> util:format("~pnd",[Num]);
    "3" -> util:format("~prd",[Num]);
    _ -> util:format("~pth",[Num])
  end.

%% @doc Check whether a given pattern is a valid regex pattern or not.
%% returns true if valid, false otherwise.
check_and_report_error(Count, PtrnName, Ptrn, Options) ->
  case is_list(Ptrn) of
    false -> util:print_error(int_to_ordinal(Count) ++ " Pointcut -- ~s, <~p>, is not a string.", [PtrnName, Ptrn], Options), true;
    true -> case re:compile(Ptrn) of
              {ok, _} -> false;
              {error, ErrSpec} ->
                util:print_error(int_to_ordinal(Count) ++ " Pointcut -- ~s, <~p>, is not a valid regex string. ERROR: ~p", [PtrnName, Ptrn, ErrSpec], Options),
                true
            end
  end.

mf_check_and_report_errors(Count, MPtrn, FPtrn, Options) ->
  HasErrorsM = check_and_report_error(Count, "The Module Pattern", MPtrn, Options),
  HasErrorsF = check_and_report_error(Count, "The Function Pattern", FPtrn, Options),
  HasErrorsM or HasErrorsF.

check_advice_types(Count,AdviceTypes,ValidAdvices,Options) ->
  AdviceTypesSet = gb_sets:from_list(AdviceTypes),
  ValidAdvicesSet = gb_sets:from_list(ValidAdvices),
  InvalidAdvices = gb_sets:to_list(gb_sets:difference(AdviceTypesSet,ValidAdvicesSet)),
  lists:foreach(fun(AT) -> util:print_warning(int_to_ordinal(Count) ++ " Pointcut -- Advice Type, <~p>, is invalid within this context, and hence will be ignored.", [AT], Options) end,InvalidAdvices).

poincut_type_check(PointCuts, HasErrors, Opts) -> poincut_type_check(1,PointCuts, HasErrors, Opts).

poincut_type_check(_,[], HasErrors, _) -> HasErrors;
poincut_type_check(Count,[{pointcut, send, M, F, Msg, Adv} | T], HasErrors, Options) ->
  check_advice_types(Count,Adv,[before,'after',intercept],Options),
  HasErrorsMF = mf_check_and_report_errors(Count, M, F, Options),
  HasErrorsMsg = check_and_report_error(Count, "The Send Message Pattern", Msg, Options),
  poincut_type_check(Count+1,T, HasErrors orelse HasErrorsMF orelse HasErrorsMsg, Options);

poincut_type_check(Count,[{pointcut, 'receive', M, F, Msg, Adv} | T], HasErrors, Options) ->
  check_advice_types(Count,Adv,[before,'after',intercept,upon],Options),
  HasErrorsMF = mf_check_and_report_errors(Count, M, F, Options),
  HasErrorsMsg = check_and_report_error(Count, "The Receive Message Pattern", Msg, Options),
  poincut_type_check(Count+1,T, HasErrors orelse HasErrorsMF orelse HasErrorsMsg, Options);

poincut_type_check(Count,[{pointcut, call, M, F, MFA, Adv} | T], HasErrors, Options) ->
  check_advice_types(Count,Adv,[before,'after',intercept],Options),
  HasErrorsMF = mf_check_and_report_errors(Count, M, F, Options),
  HasErrorsMFA = case MFA of
                   [CM, CF, A] ->
                     HasErrorsCM = check_and_report_error(Count, "The Calling Module Name pattern", CM, Options),
                     HasErrorsCF = check_and_report_error(Count, "The Calling Function pattern", CF, Options),
                     HasErrorsA = check_and_report_error(Count, "The Arity of the Calling Function", A, Options),
                     HasErrorsCM orelse HasErrorsCF orelse HasErrorsA;
                   _ ->
                     util:print_error(int_to_ordinal(Count) ++ " Pointcut -- The format of Calling MFA declaration, <~p>, is invalid. ~n\t A valid MFA structure is the following: [\"Called Module Name\", \"Called Function Name\" , \"Arity\"]", [MFA], Options),
                     true
                 end,
  poincut_type_check(Count+1,T, HasErrors orelse HasErrorsMF orelse HasErrorsMFA, Options);

poincut_type_check(Count,[{pointcut, function, M, F, A, Adv} | T], HasErrors, Options) ->
  check_advice_types(Count,Adv,[override],Options),
  HasErrorsMF = mf_check_and_report_errors(Count, M, F, Options),
  HasErrorsD = check_and_report_error(Count, "The Arity Pattern", A, Options),
  poincut_type_check(Count+1,T, HasErrors orelse HasErrorsMF orelse HasErrorsD, Options).

%% @spec read_eaop(Files::list()) -> list(#aspect{})
%% @throws nothing
%% @doc reads eaop file and converts its content to Erlang term.
%%
read_eaop(Files, Options) ->
  Pc = {'Pointcut', fun(T, M, F, A, Adv) -> {pointcut, T, M, F, A, Adv} end},
  Bindings = [Pc],
  read_eaop(Files, Bindings, [], Options).

%% @spec read_eaop(Files::list(), Bindings::list(), Result::list()) -> list(#aspect{})
%% @throws nothing
%% @doc reads eaop file and converts its content to Erlang term.
%%
read_eaop([], _, Result, _) -> Result;
read_eaop([File | Files], Bindings, Result, Options) ->
 case file:script(File, Bindings) of
    {ok, S} ->
      read_eaop(Files, Bindings, lists:append(S, Result), Options);
    {error, Error} ->
      util:print_error("Invalid .eaop -- ~p~n~p~n~p~n", [Error, File, Bindings], Options),
      read_eaop(Files, Bindings, Result, Options)
  end.

get_outdir(Options) ->
  case lists:filter(fun(Opt) -> case Opt of {outdir, _} -> true; _ -> false end end, Options) of
    [{outdir, Dir}] -> Dir ++ "\\";
    [] -> {ok, Dir} = file:get_cwd(), Dir ++ "\\"
  end.

print_advice_template(Options) ->
  case lists:member(gen_advice_template, Options) of
    true -> OutDir = get_outdir(Options),
      TemplateAdvice = "-module(advices). \n-author(\"Ian Cassar\"). \n-compile(export_all).
      \nbefore_advice(Type, Pid, Module, Function, Payload) -> \n\t io:format(\"~nBEFORE: {~p,~p,~p,~p,~p}\", [Type, Pid, Module, Function, Payload]).
      \nafter_advice(Type, Pid, Module, Function, Payload) -> \n\t io:format(\"~nAFTER: {~p,~p,~p,~p,~p}\", [Type, Pid, Module, Function, Payload]).
      \noverride_advice(Type, Pid, Module, Function, Payload) -> \n\t io:format(\"~nOVERRIDE: {~p,~p,~p,~p,~p}\", [Type, Pid, Module, Function, Payload]).
      \nintercept_advice(Type, Pid, Module, Function, Payload) -> \n\t io:format(\"~nINTERCEPT: {~p,~p,~p,~p,~p}\", [Type, Pid, Module, Function, Payload]).
      \nupon_advice(Type, Pid, Module, Function, Payload) -> \n\t io:format(\"~nUPON: {~p,~p,~p,~p,~p}\", [Type, Pid, Module, Function, Payload]).",
      case file:write_file(OutDir ++ "\\advices.erl", TemplateAdvice, [exclusive]) of
        {error, eexist} -> util:print_warning("advices.erl already exists.", [], Options);
        _ -> ok
      end;
    false -> ok
  end.

compile_advices(Options) ->
  case lists:member(compile_advices, Options) of
    true -> OutDir = get_outdir(Options),
      compile:file(OutDir ++ "\\advices.erl", Options);
    false -> ok
  end.