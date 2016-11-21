%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Oct 2016 6:08 PM
%%%-------------------------------------------------------------------
-module(server).
-author("Ian Cassar").

%% API
-export([start/0,hi/0]).

%start() -> spawn(fun math_loop/0).
start() -> spawn(fun() -> math_loop() end).

hi() -> ok.

math_loop() ->
  Y = receive
    {add, Pid, N1, N2} ->
      hi(),
      X = Pid ! {res, N1 + N2},
      io:format("X:~p",[X]);
    {sub, Pid, N1, N2} ->
      hi(),
      Pid ! {res, N1 + N2};
    {mul, Pid, N1, N2} ->
      hi(),
      Pid ! {res, N1 * N2}
  end,
  io:format("Y:~p",[Y]),
  math_loop().