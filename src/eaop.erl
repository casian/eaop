-module(eaop).
-author("Ian Cassar").

%%
%% Import modules
%%
-import(compile, []).
-import(filelib, []).
-import(file,  []).
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
  Config = get_configurations(Config_dirs, []),
  Opts = lists:append(
    [{parse_transform, weaver}, return, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(BeamFile) -> compile_beam_file(BeamFile, Opts) end, Beam_File_list),
  lists:foreach(fun(SrcFile) -> compile_src_file(SrcFile, Opts) end, Src_File_list).

write_to_file(ModuleName, Bin, OutDir) ->
  write_to_file(ModuleName, Bin, OutDir,"beam").
write_to_file(ModuleName, Bin, OutDir,Ext) ->
  FileName = io_lib:format("~s\\~p.~s", [OutDir, ModuleName,Ext]),
  file:write_file(FileName,Bin,[write]).

compile_beam_file(BeamFile, Options) ->
  [{outdir, OutDir}] = lists:filter(fun(Opt) -> case Opt of {outdir, _} -> true; _ -> false end end, Options),
  {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(BeamFile, [abstract_code]),
  case compile:forms(AC, Options) of
    {ok, ModuleName, Bin} ->
      write_to_file(ModuleName, Bin, OutDir),
      io:format("~n>> Module ~p was instrumented. ~n", [ModuleName]),
      {ok, ModuleName};
    {ok, ModuleName, Bin,_Warnings} ->
      write_to_file(ModuleName, Bin, OutDir),
      io:format("~n>> Module ~p was instrumented. ~n", [ModuleName]),
      {ok, ModuleName};
    error -> io:format("error~n", []);
    {error, Errors, Warnings} ->
      io:format("~n~n Errors = ~p Warnings = ~p~n", [Errors, Warnings]),
      {error, Errors, Warnings}
  end.

compile_src_file(File, Options) ->
  case compile:file(File, Options) of
    {ok, ModuleName} ->
      io:format("~n>> Module ~p was instrumented.", [ModuleName]),
      {ok, ModuleName};
    %%{ok, ModuleName, Warnings} -> io:format("Module name = ~p is weaved.\t Warnings = ~p~n", [ModuleName, Warnings]);
    {ok, ModuleName, _Warnings} ->
      io:format("~n>> Module ~p was instrumented. ~n", [ModuleName]),
      {ok, ModuleName};
    error -> io:format("error~n", []);
    {error, Errors, Warnings} ->
      io:format("~n~n Errors = ~p Warnings = ~p~n", [Errors, Warnings]),
      {error, Errors, Warnings}
  end.

instrument_srcs([], _, _) -> ok;
instrument_srcs(File_list, Config_dirs, Options) ->
  %%io:format("Sources = ~p ~n", [File_list]),
  Config = get_configurations(Config_dirs, []),
  Opts = lists:append(
    [{parse_transform, weaver}, return, report, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(SrcFile) -> compile_src_file(SrcFile, Opts) end, File_list).


instrument_beams([], _, _) -> ok;
instrument_beams(File_list, Config_dirs, Options) ->
  %%io:format("Sources = ~p ~n", [File_list]),
  Config = get_configurations(Config_dirs, []),
  Opts = lists:append(
    [{parse_transform, weaver}, return, report, {aop_config, Config}],
    Options
  ),
  lists:foreach(fun(BeamFile) -> compile_beam_file(BeamFile, Opts) end, File_list).


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
get_configurations([], Result) -> Result;
get_configurations([Dir | Dirs], Result) ->
  ADF_files = filelib:fold_files(Dir, ".*\.eaop$", true, fun(F, Acc) -> [F | Acc] end, []),
  io:format("Config files  = ~p~n", [ADF_files]),
  get_configurations(Dirs, lists:append(read_adf(ADF_files), Result))
.

%% @spec read_adf(Files::list()) -> list(#aspect{})
%% @throws nothing
%% @doc reads adf file and converts its content to Erlang term.
%%
read_adf(Files) ->
  Pc = {'Pointcut', fun(M, F, A, S, Adv) -> {pointcut, M, F, A, S, Adv} end},
  Bindings = [Pc],
  read_adf(Files, Bindings, [])
.

%% @spec read_adf(Files::list(), Bindings::list(), Result::list()) -> list(#aspect{})
%% @throws nothing
%% @doc reads adf file and converts its content to Erlang term.
%%
read_adf([], _, Result) -> Result;
read_adf([File | Files], Bindings, Result) ->
  case file:script(File, Bindings) of
    {ok, S} ->
      read_adf(Files, Bindings, lists:append(S, Result));
    {error, Error} ->
      io:format("Read adf error = ~p~n~p~n~p~n", [Error, File, Bindings]),
      read_adf(Files, Bindings, Result)
  end
.
