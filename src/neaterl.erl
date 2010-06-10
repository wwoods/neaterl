-module(neaterl).
-export([ compile/1, compile/2, file/1, file/2, string/2, string/3, convert/1 ]).

-include_lib("eunit/include/eunit.hrl").

%Walt Woods, 4 June 2010
%Idea that erlang can be neat and tidy ... Python-inspired indented syntax.
%No more '.' for function ends
%No more 'end' keyword at all (though it can be used, it isn't parsed)
%No more ',' for "Then do this" (Unless two statements are on the same line)
%No more ';' for "else"
%TODO: Pipe char '|' - Pipes output of A to B
%TODO: Auto line-carry when next line starts with ',', '+', '-', '++', '--', '*', '/', '|'
%TODO: Read and respond to blogs
%TODO: Strip blank lines, unroll statements before convert()
%TODO: Disallow going from an inline clause to an indented one

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
  
compile(File,debug) when is_atom(File) ->
  compile(atom_to_list(File),debug)
  ;
compile(File,debug) ->
  {ok,Module} = file(File,debug)
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
  In = Name ++ ".erln"
  ,Out = Name ++ ".erl"
  ,Text = lists:flatten(readlines(In))
  ,string(Text, Out)
  .
  
file(Name, debug) ->
  In = Name ++ ".erln"
  ,Out = Name ++ ".erl"
  ,Text = lists:flatten(readlines(In))
  ,string(Text, Out, debug)
  .
  
string(String, Outfile) ->
  {ok,B,_}=neaterl_lex:string(String)
  ,{ok,L}=convert_indents(B)
  ,{ok,Y}=neaterl_yec:parse(L)
  ,C=convert(Y)
  ,writefile(Outfile, C)
  .
  
string(String, Outfile, debug) ->
  {ok,B,_}=neaterl_lex:string(String)
  ,{ok,L}=convert_indents(B)
  ,io:format("Lexed: ~p~n", [L])
  ,{ok,Y}=neaterl_yec:parse(L)
  ,io:format("Parsed: ~p~n", [Y])
  ,C=convert(Y)
  ,io:format("Output:~n~s~n", [C])
  ,writefile(Outfile, C)
  .

reload(Module) ->
  {ok,ModName} = compile:file(Module)
  ,code:purge(ModName)
  ,{module,ModName} = code:load_file(Module)
  ,ok
  .
  
convert_indents(List) ->
  Out=convert_indents([], [""], List)
  ,{ok,Out}
  .
  
convert_indents(Out, [Cur,Next|Indents], []) ->
  convert_indents(Out ++ [{'end', element(2, lists:last(Out)), ""}], [Next] ++ Indents, [])
  ;
convert_indents(Out, [Single|_], []) ->
  Out
  ;
convert_indents(Out, Indents, [{indent,Line,New},{indent,NextLine,NextText}|T]) ->
  convert_indents(Out, Indents, [{indent,NextLine,NextText}] ++ T)
  ;
convert_indents(Out, [Cur|Indents], [{indent,Line,New}|T]) ->
  if
    length(New) > length(Cur) -> convert_indents(Out ++ [{'begin',Line,New}], [New] ++ [Cur] ++ Indents, T)
    ;length(T) == 0, length(New) == length(Cur) -> convert_indents(Out, [Cur] ++ Indents, T)
    ;length(New) == length(Cur) -> convert_indents(Out ++ [{line,Line}], [Cur] ++ Indents, T)
    ;true -> convert_indents(Out ++ [{'end',Line,New}], Indents, [{indent,Line,New}] ++ T)
  end
  ;
convert_indents(Out, Indents, [H|T]) ->
  convert_indents(Out ++ [H], Indents, T)
  .
  
convert_output(Out, Line, Indent, []) ->
  Out
  ;
convert_output(Out, Line, Indent, [{line, nil}|T]) ->
  convert_output(Out, Line, Indent, T)
  ;
convert_output(Out, Line, Indent, [{line, L}|T]) ->
  if 
    L > Line -> 
      convert_output(Out ++ lists:flatten(lists:map(fun(X) -> "\n" end, lists:seq(Line+1, L)))
        ++ Indent
        , L, Indent, T
        )
    ;L == Line -> convert_output(Out, Line, Indent, T)
    end
  ;
convert_output(Out, Line, Indent, [{indent, I}|T]) ->
  convert_output(Out, Line, I, T)
  ;
convert_output(Out, Line, Indent, [H|T]) when is_integer(H) ->
  convert_output(Out ++ [ H ], Line, Indent, T)
  .

convert({module, Name, Exports, Stmts}) ->
  Out=lists:flatten([ 
    io_lib:format("-module(~s).", [Name])
    ,{ line, 2 }
    ,"-export(["
    , convert_stmts(",", Exports)
    , io_lib:format("]).", [])
    , { line, 3 }
    , convert("", Stmts)
    ])
  ,convert_output("", 1, "", Out)
  .
  
convert_stmts(Stmts) ->
  convert_stmts("", Stmts)
  .

convert_stmts(Delimit, []) ->
  ""
  ;
convert_stmts(Delimit, Stmts) when not is_list(Stmts) ->
  convert("", [ Stmts ])
  ;
convert_stmts(Delimit, Stmts) when is_list(Delimit) ->
  convert_stmts({constant, nil, Delimit}, Stmts)
  ;
convert_stmts(Delimit, Stmts) ->
  convert("", list_insert(Delimit, Stmts))
  .

convert(Out, []) ->
  Out
  ;
convert(Out, [H|T]) when element(1, H) == 'begin'; element(1, H) == 'end' ->
  convert(Out ++ convert2(H, nil), T)
  ;
convert(Out, [H,Peek|T]) ->
  convert(Out ++ [ { line, element(2, H) } ] ++ convert2(H, Peek), [ Peek ] ++ T)
  ;
convert(Out, [H|T]) ->
  convert(Out ++ [ { line, element(2, H) } ] ++ convert2(H, nil), T)
  .
  
convert2({export,_Line,Func,ArgCount}, Next) ->
  io_lib:format("~s/~p", [ Func, ArgCount ])
  ;
convert2({'begin',Line,Indent}, Next) ->
  [ { indent, Indent}, { line, Line } ]
  ;
convert2({'end',Line,Indent}, Next) ->
  [ { indent, Indent} ]
  ;
convert2({constant,_Line,String}, Next) ->
  String
  ;
convert2({function_def,_Line,Name,Body}, Next) ->
  Term = case function_def_match({function_def,_Line,Name,Body}, Next) of
    true -> ";"
    ;false -> "."
    end
  ,Name ++ convert_stmts(Body) ++ Term
  ;
convert2({function_body, _, Args, When, Body}, Next) ->
  WhenPart = if
    When == nil -> ""
    ;true -> " when " ++ convert_stmts(When)
    end
  ,convert_stmts(Args) ++ WhenPart ++ " -> " ++ convert_stmts(",", Body)
  ;
convert2({arg_list, _, Args}, Next) ->
  "(" ++ convert_stmts(", ", Args) ++ ")"
  ;
convert2({funccall, Line, Names, Args}, Next) ->
  convert_stmts(":", Names) ++ convert_stmts(Args)
  ;
convert2({'case', Line, Expr, Branches}, Next) ->
  "case " ++ convert_stmts(Expr) ++ " of " ++ convert_stmts("; ", Branches) ++ " end"
  ;
convert2({'if', Line, Branches}, Next) ->
  "if " ++ convert_stmts("; ", Branches) ++ " end"
  ;
convert2({'receive', Line, Branches}, Next) ->
  "receive " ++ convert_stmts("; ", Branches) ++ " end"
  ;
convert2({branch, Line, Expr, Stmts}, Next) ->
  convert_stmts(Expr) ++ " -> " ++ convert_stmts(", ", Stmts)
  ;
convert2({'after', Line, Expr, Stmts}, Next) ->
  "after " ++ convert_stmts(Expr) ++ " -> " ++ convert_stmts(", ", Stmts)
  ;
convert2({'fun', Line, Clauses}, Next) ->
  "fun " ++ convert_stmts("; ", Clauses) ++ " end"
  ;
convert2({macro, Line, Name, nil}, Next) ->
  Name
  ;
convert2({macro, Line, Name, Args}, Next) ->
  Name ++ convert_stmts(Args)
  ;
convert2({binary_op, Line, Symbol, Left, Right}, Next) ->
  convert_stmts(Left) ++ Symbol ++ convert_stmts(Right)
  ;
convert2({unary_op, Line, Symbol, Right}, Next) ->
  Symbol ++ convert_stmts(Right)
  ;
convert2({ list, Line, Args, Tail }, Next) ->
  TailPart = case Tail of
    nil -> ""
    ;V -> "|" ++ convert_stmts(Tail)
    end
  ,"[" ++ convert_stmts(", ", Args) ++ TailPart ++ "]"
  ;
convert2({ tuple, Line, Args }, Next) ->
  "{" ++ convert_stmts(", ", Args) ++ "}"
  ;
convert2({ paren_expr, Line, Expr }, Next) ->
  "(" ++ convert_stmts(Expr) ++ ")"
  ;
convert2(H, Next) ->
  io_lib:format("<Unknown ~p>", [ element(1, H) ])
  .
  
function_def_match(
  {function_def, _, Name1, {function_body,_,{arg_list,_,Args1},_,_}}
  , {function_def, _, Name2, {function_body,_,{arg_list,_,Args2},_,_}}) ->
  case
    Name1 == Name2 andalso list_length(Args1) == list_length(Args2) 
  of
    true -> true
    ;_ -> false
    end
  ;
function_def_match({function_def, _, Name1, {function_body,_,{arg_list,_,Args1},_,_}}, B) ->
  false
  .
  
list_insert(Symbol, [H|List]) when element(1, H) == 'begin' ->
  [_|Result] = list_insert([], Symbol, List)
  ,[H] ++ Result
  ;
list_insert(Symbol, List) ->
  [_|Result] = list_insert([], Symbol, List)
  ,Result
  .
  
list_insert(Out, Symbol, []) ->
  Out
  ;
list_insert(Out, Symbol, [H|T]) when element(1, H) == 'end'; element(1, H) == 'after' ->
  list_insert(Out ++ [H], Symbol, T)
  ;
list_insert(Out, Symbol, [H|T]) ->
  list_insert(Out ++ [ Symbol ] ++ [H], Symbol, T)
  .
  
list_length(List) ->
  list_length(0, List)
  .
  
list_length(Out, []) ->
  Out
  ;
list_length(Out, [H|T]) when element(1, H) == 'begin'; element(1, H) == 'end' ->
  list_length(Out, T)
  ;
list_length(Out, [H|T]) ->
  list_length(Out + 1, T)
  .

%% Neat Erl Tests

compile_test() ->
  {ok,J} = compile("examples/example")
  ,6=J:fac(3)
  %Lookup eunit docs for more...
  .
