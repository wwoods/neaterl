-module(factorial).
-export([fac/1,test/1,comm/0,comm/1,yecc/0]).




fac(0) -> 1;
fac(N) when (is_integer(N)),((N)>(0)) -> 
  (N)*(fac((N)-(1))).

-define(MOD(), 36).

test(0) -> 
  put(myatom,"hello world")
  ,erlang:display(get(myatom))
  ,case true of 
    false -> never
    ;true -> always end;

test(N) -> 
  if 
    (N)==(1) -> true
    ;true -> (?MODULE:test((N)-(1)))*(?MOD()) end.

comm() -> 
  io:format('I am ~p~n',[self()])
  ,(PId)=(spawn(fac,comm,[true]))
  ,erlang:monitor(process,PId)
  ,io:format('~p created ~p~n',[self(),PId])
  ,erlang:display(PId ! {bullocks})
  ,erlang:display(PId ! {kill,5})
  ,receive 
    {'DOWN',Ref,process,Pid2,Reason} -> 
      io:format('~p Exit Detected: ~s~n',[self(),Reason]) end.

comm(true) -> 
  receive 
    {kill,N} -> 
      io:format('~p ~s ~p~n',[self(),'Kill received with ',N])
      ,receive 
        {bullocks} -> 
          io:format('~p ~s~n',[self(),'Bullocks received after kill'])
          ,erlang:exit('Bullocks interrupted')

        after (N)*(1000) -> 
          io:format('~p ~s~n',[self(),'Kill executed'])
          ,erlang:exit(normal) end end.

reload(Module) -> 
  compile:file(Module)
  ,code:purge(Module)
  ,code:load_file(Module)
  ,ok.

yecc() -> 
  yecc:file(test_parse)
  ,reload(test_parse)
  ,leex:file(test_lex)
  ,reload(test_lex)
  ,(B)=(element(2,test_lex:string("[]{}1.4142\n   3.145\n8")))
  ,io:format("Tokenized: ~p~n",[B])
  ,test_parse:parse(B).