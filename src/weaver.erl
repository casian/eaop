-module(weaver).

-include("aop.hrl").

-export([parse_transform/2, msg_to_string/1]).

msg_to_string(Msg) ->
  re:replace(parse_msg(Msg), "\\s+", "", [global, {return, list}]).

parse_msg({tuple, _, TupleContent}) ->
  "{" ++ parse_msg(TupleContent) ++ "}";
parse_msg({atom, _, Atom}) ->
  atom_to_list(Atom);
parse_msg({string, _, Str}) ->
  "\"" ++ Str ++ "\"";
parse_msg({op, _, _}) -> "_";
parse_msg({var, _, _}) -> "_";
parse_msg(List) when is_list(List) ->
  Str = lists:foldl(fun(Elem, Acc) -> Acc ++ "," ++ parse_msg(Elem) end, "", List),
  string:strip(Str, both, 44);
%parse_msg(N) when is_number(N)-> N;
parse_msg(_) -> "_".

parse_transform(Forms, Options) ->
  Defs = proplists:get_value(aop_config, Options, []),
  try
    [_H | [H | _T]] = Forms,
    {_, _, _, _} = H,
    %%FileName = "weaver_output_" ++ atom_to_list(Program) ++ ".erl",
    %%{ok, File} = file:open(FileName, [write]),
    NewCallForm = weaver_call:handle_call(Forms, Defs),
    NewSendingForm = weaver_send:handle_send(NewCallForm, Defs),
    NewReceivingForm = weaver_receive:handle_receive(NewSendingForm, Defs),
    NewInitForm = weaver_override:handle_override(NewReceivingForm,Defs),

    %%IOList = [erl_pp:form(X1) || X1 <- Forms],
    %%io:format(File,"~p", [IOList]),

    NewInitForm
  catch
    error:Error -> io:format("Exception = ~p StackTrace: ~p ~n", [Error, erlang:get_stacktrace()]),
      Forms
  end.