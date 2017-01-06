%%%-------------------------------------------------------------------
%%% @author Ian Cassar
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2016 3:55 PM
%%%-------------------------------------------------------------------
-module(util).
-author("Ian Cassar").

%% API
-export([regex_match/2,format/1,format/2,print_error/3,print_msg/3,print_warning/3]).

regex_match(Subject, RE) ->
  IsMatch = re:run(Subject, RE),
  case IsMatch of
    nomatch -> false;
    {match, _} -> true
  end.

format(String) ->
  String.
format(String, Params) ->
  lists:flatten(io_lib:format(String, Params)).

print_error(Format,Args,Options) ->
  io:format("ERROR: "++Format++"~n",Args).

print_warning(Format,Args,Options) ->
  io:format("WARNING: "++Format++"~n",Args).

print_msg(Format,Args,Options) ->
  io:format(Format,Args).