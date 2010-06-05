-module(prettyerl).
-export([ test/0 ]).

%Walt Woods, 4 June 2010

reload(Module) ->
  compile:file(Module)
  ,code:purge(Module)
  ,code:load_file(Module)
  ,ok
  .

test() ->
  leex:file(prettyerl_lex)
  ,reload(prettyerl_lex)
  ,yecc:file(prettyerl_yec)
  ,reload(prettyerl_yec)
  ,B=element(2, prettyerl_lex:string("-module(test)\n-export([hello_world/0])\n\nhello_world"))
  ,io:format("Tokenized: ~p~n", [B])
  ,prettyerl_yec:parse(B)
  .
