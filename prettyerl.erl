-module(prettyerl).
-export([ test/0, compile/1, file/1, file/2, string/2, string/3, convert/1 ]).

%Walt Woods, 4 June 2010
%Idea that erlang can be pretty... Python-inspired indented syntax.
%No more '.' for function ends
%No more 'end' keyword at all (though it can be used, it isn't parsed)
%No more ',' for "Then do this" (Unless two statements are on the same line)
%No more ';' for "else"
%TODO: Pipe char '|' - Pipes output of A to B
%TODO: Auto line-carry when next line starts with ',', '+', '-', '++', '--', '*', '/', '|'
%TODO: Read and respond to blogs
%TODO: Strip blank lines, unroll statements before convert()
%TODO: Anonymous functions

%Read file from http://wiki.trapexit.erlang-consulting.com/Read_File_to_List
readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read])
  ,get_all_lines(Device, "")
  .
  
get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    eof -> file:close(Device), Accum
    ;Line -> get_all_lines(Device, Accum ++ [Line])
  end
  .
  
writefile(File, Text) ->
  {ok, IODevice} = file:open(File, [write])
  ,file:write(IODevice, Text)
  ,file:close(IODevice)
  ,{ok,File}
  .
  
compile(File) when is_atom(File) ->
  compile(atom_to_list(File))
  ;
compile(File) ->
  {ok,Module} = file(File)
  ,case compile:file(Module) of
    {ok,ModName} ->
      code:purge(ModName)
      ,code:load_file(ModName)
      ,{ok,ModName}
    ;error -> error
  end
  .

file(Atom) when is_atom(Atom) ->
  file(atom_to_list(Atom))
  ;
file(Name) ->
  In = Name ++ ".erlp"
  ,Out = Name ++ ".erl"
  ,Text = lists:flatten(readlines(In))
  ,string(Text, Out)
  .
  
file(Name, debug) ->
  In = Name ++ ".erlp"
  ,Out = Name ++ ".erl"
  ,Text = lists:flatten(readlines(In))
  ,string(Text, Out, debug)
  .
  
string(String, Outfile) ->
  {ok,L,_}=prettyerl_lex:string(String)
  ,{ok,Y}=prettyerl_yec:parse(L)
  ,C=convert(Y)
  ,writefile(Outfile, C)
  .
  
string(String, Outfile, debug) ->
  {ok,L,_}=prettyerl_lex:string(String)
  ,io:format("Lexed: ~p~n", [L])
  ,{ok,Y}=prettyerl_yec:parse(L)
  ,io:format("Parsed: ~p~n", [Y])
  ,C=convert(Y)
  ,io:format("Output:~n~s~n", [C])
  ,writefile(Outfile, C)
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
  ,B=element(2, prettyerl_lex:string("-module(test)\n-export([hello_world/0,fac/1])\n\nhello_world() -> io:format(\"~p~n\", \"Hello, world!\")\n\nfac(0) -> 1\nfac(N) -> \n  N * fac(N-1)\n\nblah(N) when is_atom(N) -> whentop\nblah(N) -> is_integer(N), N>0"))
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
convert(Out, [{nonindent,IFlags}|Indents], NewFlags, [{{indent,Line,New},Stmts}|T]) ->
  %Handles when an indent was a single line
  convert(Out ++ convert_deindent(IFlags)
    , Indents, NewFlags, [{{indent,Line,New},Stmts}] ++ T)
  ;
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
  convert(Out, [{nonindent, [first] ++ IFlags}] ++ Indents, nil, [Stmt] ++ T)
  ;
convert(Out, Indents, NewFlags, [{{indent,Line,Indent},Stmts}|T]) ->
  convert(Out ++ [ { goto_line, Line, Indent } ], Indents, NewFlags, Stmts ++ T)
  ;
% HANDLE STATEMENTS
convert(Out, [{Cur,[first|Rest]}|Indents], NewFlags, Any) ->
  convert_stmt(Out, [{Cur,Rest}] ++ Indents, NewFlags, Any)
  ;
convert(Out, Indents, NewFlags, [{'after',Timeout,Stmts}|T]) ->
  %ODDITY HANDLING: after clauses don't use level flags..
  convert_stmt(Out, Indents, NewFlags, [{'after',Timeout,Stmts}] ++ T)
  ;
convert(Out, Indents, NewFlags, Any) ->
  convert_stmt(Out ++ convert_level_flags(Indents), Indents, NewFlags, Any)
  .
  
convert_stmt(Single) when not is_list(Single) ->
  convert_stmt("", [], [], [ Single ])
  ;
convert_stmt(Single) ->
  convert_stmt("", [{"",[]}], [], Single )
  .

%Looks for a new function definition; unwraps indent loops
next_func_def([]) -> nil
;next_func_def([{{indent,_,_},Stmts}|T]) -> next_func_def(Stmts ++ T)
;next_func_def([{function_def,Name,Args,When}|T]) -> {function_def,Name,Args,When}
;next_func_def([H|T]) -> next_func_def(T)
.
  
convert_stmt(Out, Indents, NewFlags, [{function_def,Name,Args,When}|T]) ->
  EndType = case next_func_def(T) of
      {function_def,Name,Args2,_} when length(Args) == length(Args2) ->
        func_sep
      ;_ -> func_end
    end
  ,
  WhenPart = case When of
      nil -> ""
      ;_ -> " when " ++ convert_stmt(When)
    end
  ,
  convert(Out ++ io_lib:format("~p(", [ Name ]) 
    ++ convert_arglist(Args) ++ ")" ++ WhenPart ++ " -> "
    , Indents, {indent,[comma,EndType]}
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{'case', Expr}|T]) ->
  convert(Out ++ "case " ++ convert_stmt(Expr) ++ " of "
    , Indents, {indent,[semicolon,'end']}
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{'if'}|T]) ->
  convert(Out ++ "if "
    , Indents, {indent,[semicolon,'end']}
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{branch_condition,Expr}|T]) ->
  convert(Out ++ convert_stmt(Expr) ++ " -> "
    , Indents, { indent,[comma] }
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{'send', Proc, Message}|T]) ->
  convert(Out ++ convert_stmt(Proc) ++ " ! " ++ convert_stmt(Message)
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{'receive', Stmts}|T]) ->
  convert(Out ++ "receive "
    , Indents, { indent, [semicolon,'end'] }
    , Stmts ++ T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{'after', Timeout, Stmts}|T]) ->
  convert(Out ++ "after " ++ convert_stmt(Timeout) ++ " -> "
    , Indents, { indent, [comma] }
    , Stmts ++ T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{funccall, Name, Args}|T]) ->
  convert(Out ++ convert_stmt(list_insert({ constant, ":" }, Name)) ++ "(" 
    ++ convert_arglist(Args)
    ++ ")"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{binary_op,Op,Left,Right}|T]) ->
  convert(Out ++ "(" ++ convert_stmt(Left) ++ ")" ++ Op ++ "(" ++ convert_stmt(Right) ++ ")"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{unary_op,Op,Left}|T]) ->
  convert(Out ++ Op ++ "(" ++ convert_stmt(Left) ++ ")"
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
convert_stmt(Out, Indents, NewFlags, [{tuple, Args}|T]) ->
  convert(Out ++ "{" ++ convert_arglist(Args) ++ "}"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{macro, Symbol, nil}|T]) ->
  convert(Out ++ Symbol
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{macro, Symbol, Args}|T]) ->
  convert(Out ++ Symbol ++ "(" ++ convert_arglist(Args) ++ ")"
    , Indents, NewFlags
    , T
    )
  ;
convert_stmt(Out, Indents, NewFlags, [{constant,Text}|T]) ->
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
convert_level_flags2(Out, [semicolon|T]) ->
  convert_level_flags2(Out ++ ";", T)
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
convert_deindent(Out, [func_sep|T]) ->
  convert_deindent(Out ++ ";", T)
  ;
convert_deindent(Out, [func_end|T]) ->
  convert_deindent(Out ++ ".", T)
  ;
convert_deindent(Out, ['end'|T]) ->
  convert_deindent(Out ++ " end", T)
  ;
convert_deindent(Out, [semicolon|T]) ->
  %Level flag - do nothing
  convert_deindent(Out, T)
  ;
convert_deindent(Out, [comma|T]) ->
  %Level flag - do nothing
  convert_deindent(Out, T)
  .
  
list_insert(Symbol, List) ->
  [_|Result] = list_insert([], Symbol, List)
  ,Result
  .
  
list_insert(Out, Symbol, []) ->
  Out
  ;
list_insert(Out, Symbol, [H|T]) ->
  list_insert(Out ++ [ Symbol ] ++ [H], Symbol, T)
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
  