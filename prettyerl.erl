-module(prettyerl).
-export([ test/0, file/1, string/2, convert/1 ]).

%Walt Woods, 4 June 2010
%Idea that erlang can be pretty... Python-inspired indented syntax.
%No more '.' for function ends
%No more ',' for "Then do this"
%No more ';' for "else"
%end keyword is optional
%TODO: Pipe char '|' - Pipes output of A to B
%TODO: Auto line-carry when next line starts with ',', '+', '-', '++', '--', '*', '/', '|'
%TODO: Read and respond to blogs

file(Name) ->
  notimplemented
  .
  
string(String, Outfile) ->
  L=element(2, prettyerl_lex:string(String))
  ,{ok,Y}=prettyerl_yec:parse(L)
  ,C=convert(Y)
  .

reload(Module) ->
  compile:file(Module)
  ,code:purge(Module)
  ,code:load_file(Module)
  ,ok
  .

test() ->
  {ok,_} = leex:file(prettyerl_lex)
  ,ok = reload(prettyerl_lex)
  ,{ok,_} = yecc:file(prettyerl_yec)
  ,ok=reload(prettyerl_yec)
  ,B=element(2, prettyerl_lex:string("-module(test)\n-export([hello_world/0,fac/1])\n\nhello_world() -> io:format(\"~p~n\", \"Hello, world!\")\n\nfac(0) -> 1\nfac(N) -> \n  N * fac(N-1)"))
  ,io:format("Tokenized: ~p~n", [B])
  ,{ok,P}=prettyerl_yec:parse(B)
  ,io:format("Parsed: ~p~n", [P])
  ,io:format("Final:~n~s~n", [ convert(P) ])
  ,all_tests_passed
  .
  
convert_goto_line(CurOut, CurLine, []) -> 
  CurOut
  ;
convert_goto_line(CurOut, CurLine, [{goto_line, Line, Indent}|T]) when CurLine == Line ->
  convert_goto_line(CurOut ++ Indent, CurLine, T)
  ;
convert_goto_line(CurOut, CurLine, [{goto_line, Line, Indent}|T]) when CurLine < Line ->
  convert_goto_line(CurOut ++ "\n", CurLine + 1, [{goto_line, Line, Indent}] ++ T)
  ;
convert_goto_line(CurOut, CurLine, [H|T]) ->
  convert_goto_line(CurOut ++ [H], CurLine, T)
  
.
  

convert({module, Name, Exports, Stmts}) ->
  Out=lists:flatten([ 
    io_lib:format("-module(~s).", [Name])
    ,{ goto_line, 2, "" }
    ,"-export(["
    , convert_arglist(Exports)
    , io_lib:format("]).", [])
    , { goto_line, 3, "" }
    ])
  ,Out2 = lists:flatten(convert(Out, [{"",[]}], nil, Stmts))
  ,convert_goto_line("", 1, Out2)
  .
  
convert_arglist(List) ->
  convert_arglist("", comma_delimit(List))
  .
  
convert_arglist(Out, []) ->
  Out
  ;
convert_arglist(Out, [{export,Func,ArgCount}|T]) ->
  convert_arglist(Out ++ io_lib:format("~s/~p", [ Func, ArgCount ]), T)
  ;
convert_arglist(Out, [','|T]) ->
  convert_arglist(Out ++ ",", T)
  ;
convert_arglist(Out, [Other|T]) ->
  %Dirty shortcut to prevent code duplication; should be ok though,
  %as yecc will only allow certain statement types to pass through.
  convert_arglist(Out ++ convert_stmt("", [], [], [Other]), T)
  .
  
% MAIN PARSER
convert(Out, [], NewFlags, []) ->
  Out
  ;
convert(Out, [{_,IFlags}|Indents], NewFlags, []) ->
  convert(Out ++ convert_deindent(IFlags), Indents, NewFlags, [])
  ;
convert(Out, Indents, NewFlags, [{{indent,Line,Indent},[]}|T]) ->
  %Ignore blank lines
  convert(Out, Indents, NewFlags, T)
  ;
convert(Out, Indents, NewFlags, [{verbatim, Output}|T]) ->
  convert(Out ++ Output, Indents, NewFlags, T)
  ;
% HANDLE INDENTATION CHANGES
convert(Out, [{Cur,F}|Indents], nil, [{{indent,Line,New},Stmts}|T]) when not (Cur == New) ->
  case lists:prefix(New, Cur) of
    false -> io_lib:format("Unexpected indent, line ~p", [ Line ])
    ;true ->
      convert(Out ++ convert_deindent("", F), Indents, nil, [{{indent,Line,New},Stmts}] ++ T)
  end
  ;
convert(Out, [{Cur,_}|_], {indent,_}, [{{indent,Line,New},_}|_]) when Cur == New ->
  io_lib:format("Expected indent, line ~p", [ Line ])
  ;
convert(Out, [{Cur,F}|Indents], {indent,IFlags}, [{{indent,Line,New},Stmts}|T]) ->
  case lists:prefix(Cur, New) of
    false -> io_lib:format("Expected indent, line ~p", [ Line ])
    ;true ->
      convert(
        Out ++ [ { goto_line, Line, New } ]
        , [{New,[first] ++ IFlags}] ++ [{Cur,F}] ++ Indents
        , nil
        , Stmts ++ T
        )
  end
  ;
convert(Out, Indents, {indent,IFlags}, [Stmt|T]) ->
  %Stmt is NOT {{indent,_,_},_}, so is a one-line indented clause.
  convert(Out, Indents, nil, [Stmt] ++ [{verbatim, convert_deindent("", IFlags)}] ++ T)
  ;
convert(Out, Indents, NewFlags, [{{indent,Line,Indent},Stmts}|T]) ->
  convert(Out ++ [ { goto_line, Line, Indent } ], Indents, NewFlags, Stmts ++ T)
  ;
% HANDLE STATEMENTS
convert(Out, [{Cur,[first|Rest]}|Indents], NewFlags, Any) ->
  convert_stmt(Out, [{Cur,Rest}] ++ Indents, NewFlags, Any)
  ;
convert(Out, Indents, NewFlags, Any) ->
  convert_stmt(Out ++ convert_level_flags(Indents), Indents, NewFlags, Any)
  .
  
convert_stmt(Single) when not is_list(Single) ->
  convert_stmt("", [], [], [ Single ])
  .
  
convert_stmt(Out, Indents, NewFlags, [{function_def,Name,Args}|T]) ->
  convert(Out ++ io_lib:format("~p(~s) -> ", [ Name, convert_arglist(Args) ])
    , Indents, {indent,[comma,{func, Name, length(Args)}]}
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{funccall, _, Name, Args}|T]) ->
  convert(Out ++ io_lib:format("~s(", [ Name ])
    ++ convert_arglist(Args)
    ++ ")"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{binary_op,Op,Left,Right}|T]) ->
  convert(Out ++ convert_stmt(Left) ++ Op ++ convert_stmt(Right)
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{unary_op,Op,Left}|T]) ->
  convert(Out ++ Op ++ convert_stmt(Left)
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{list, Args}|T]) ->
  convert(Out ++ "[" ++ convert_arglist(Args) ++ "]"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{Constant,Text}|T]) ->
  convert(Out ++ Text
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [Unknown|T]) ->
  convert(Out ++ io_lib:format("<Unknown - ~p>", [ element(1, Unknown) ])
    , Indents, NewFlags, T)
  .
  
convert_level_flags([{_, Flags}|_]) ->
  convert_level_flags2("", Flags)
  .

convert_level_flags2(Out, []) ->
  Out
  ;
convert_level_flags2(Out, [comma|T]) ->
  convert_level_flags2(Out ++ ",", T)
  ;
convert_level_flags2(Out, [H|T]) ->
  %Do nothing for unknown; assume it's a deindent
  convert_level_flags2(Out, T)
  .
  
convert_deindent(List) when is_list(List) ->
  convert_deindent("", List)
  .
  
convert_deindent(Out, []) ->
  Out
  ;
convert_deindent(Out, [{func, Name, ArgCount}|T]) ->
  convert_deindent(Out ++ io_lib:format("<End of ~s>", [ Name ]), T)
  ;
convert_deindent(Out, [comma|T]) ->
  %Level flag - do nothing
  convert_deindent(Out, T)
  .

comma_delimit([]) ->
  []
  ;
comma_delimit(List) ->
  L=comma_insert(List)
  ,lists:sublist(L, length(L) - 1)
  .
  
comma_insert([H|T]) ->
  [H,','] ++ comma_insert(T)
  ;
comma_insert([]) ->
  []
  .
  