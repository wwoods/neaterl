-module(neaterl).
-export([ shell/0, compile/1, compile/2, file/1, file/2, string/1, string/2, load_file/1, load_file/2, load_string/1, load_string/2, make_all/0 ]).

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
%TODO: Maybe: Disallow going from an inline clause to an indented one

-define(COMPILE_DEFAULTS, [ verbose, report ]).
-define(COMPILE_TEMP, "temp_neaterl.erl").

shell() ->
  neaterl.shell:run()
  .
  
make_all() ->
  List = load_makefile()
  ,make_all(List)
  .
  
make_all([]) -> 
  ok
  ;
make_all([{Pattern, Options}|T]) ->
  Files = filelib:wildcard(Pattern)
  ,ErlnFiles = lists:filter(fun(X) -> lists:suffix(".erln", X) end, Files)
  ,lists:map(fun(X) -> io:format("Recompile: ~s~n", [X]), compile(X, Options ++ [ report ]) end
    , lists:map(fun(X) -> lists:sublist(X, length(X) - length(".erln")) end, ErlnFiles)
    )
  ,make_all(T)
  .
  
load_makefile() ->
  [ {"src/*", [ debug_info, {outdir, "ebin"}, {i, "include"}, debug_neaterl ] } ]
  .

%Read file from http://wiki.trapexit.erlang-consulting.com/Read_File_to_List
readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read])
  ,try
    get_all_lines(Device, "")
  after
    file:close(Device)
  end
  .
  
get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    eof -> file:close(Device), Accum
    ;Line -> get_all_lines(Device, Accum ++ [Line])
  end
  .
  
writefile(File, Text) ->
  {ok, IODevice} = file:open(File, [write])
  ,try
    file:write(IODevice, Text)
    ,file:close(IODevice)
    ,{ok,File}
  after
    file:close(IODevice)
  end
  .
  
compile(File) ->
  compile(File, ?COMPILE_DEFAULTS)
  .
  
compile(File, Options) when is_atom(File) ->
  compile(atom_to_list(File), Options)
  ;
compile(File, Options) ->
  {ok,ErlCode}=file(File, Options)
  ,OutDir = proplists:get_value(outdir, Options, ".")
  ,case compile_erl(ErlCode, [ binary ] ++ Options) of
    {ok,Module,Binary} -> 
      {Path,Last} = compile_get_path(Module)
      ,filelib:ensure_dir(OutDir ++ Path)
      ,writefile(OutDir ++ Path ++ Last ++ ".beam", Binary)
      ,{ok,Module}
    ;Other -> Other
  end
  .
  
compile_get_path(Module) when is_atom(Module) ->
  compile_get_path(atom_to_list(Module))
  ;
compile_get_path(Module) ->
  Parts = lists:map(fun(X) -> binary_to_list(X) end, re:split(Module, "\\."))
  ,Last = lists:last(Parts)
  ,Parts2 = lists:sublist(Parts, length(Parts) - 1)
  ,{"/" ++ string:join(Parts2, "/") ++ "/", Last}
  .

file(File) ->
  file(File, ?COMPILE_DEFAULTS)
  .
  
file(Name, Options) when is_atom(Name) ->
  file(atom_to_list(Name), Options)
  ;
file(Name, Options) ->
  In = Name ++ ".erln"
  ,Text = lists:flatten(readlines(In))
  ,string(Text, Options)
  .
  
string(String) ->
  string(String, ?COMPILE_DEFAULTS)
  .
  
string(String, Options) ->
  {ok,B,_}=neaterl_lex:string(String)
  ,{ok,L}=convert_indents(B)
  ,proplists:is_defined(debug_neaterl, Options) andalso io:format("Tokens: ~p~n", [ L ])
  ,{ok,Y}=neaterl_yec:parse(L)
  ,proplists:is_defined(debug_neaterl, Options) andalso io:format("Parsed as: ~p~n", [ Y ])
  ,{ok,convert(Y)}
  .
  
load_file(File) ->
  load_file(File, ?COMPILE_DEFAULTS)
  .
  
load_file(File, Options) ->
  {ok,ErlCode} = file(File, Options)
  ,load_code(ErlCode, Options)
  .
  
load_string(String) ->
  load_string(String, ?COMPILE_DEFAULTS)
  .
  
load_string(String, Options) ->
  {ok,ErlCode} = string(String, Options)
  ,load_code(ErlCode, Options)
  .
  
compile_erl(ErlCode, Options) ->
  F=?COMPILE_TEMP
  ,try
    writefile(F, ErlCode)
    ,N = compile:file(F, Options)
  catch
    error:Reason -> {error,{Reason,erlang:get_stacktrace()}}
  after
    proplists:is_defined(keep_temp, Options) orelse file:delete(F)
  end
  .
  
load_code(ErlCode, Options) ->
  {ok,Module,Binary} = compile_erl(ErlCode, [ binary ] ++ Options)
  ,code:purge(Module)
  ,{module,Module} = code:load_binary(Module, "script", Binary)
  .
  
parse_forms(List) -> 
  FormGroups = split_script([], [], List)
  ,parse_forms([], FormGroups)
  .
  
parse_forms(Out, [H|T]) ->
  io:format("Form: ~p~n", [H]),
  {ok,Form} = erl_parse:parse_form(H)
  ,parse_forms(Out ++ [ Form ], T)
  ;
parse_forms(Out, []) ->
  {ok, Out}
  .
  
split_script(Out, Temp, [H={dot,Line}|T]) ->
  split_script(Out ++ [ Temp ++ [H] ], [], T)
  ;
split_script(Out, Temp, [H|T]) ->
  split_script(Out, Temp ++ [H], T)
  ;
split_script(Out, Temp, []) -> 
  Out %Always will be a dot before the end.
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
convert_indents(Out, [_Single|_], []) ->
  Out
  ;
convert_indents(Out, Indents, [{indent,Line,New},{indent,NextLine,NextText}|T]) ->
  convert_indents(Out, Indents, [{indent,NextLine,NextText}] ++ T)
  ;
convert_indents(Out, Indents, [{indent,_,_}]) ->
  convert_indents(Out, Indents, [])
  ;
convert_indents(Out, [Cur|Indents], [{indent,Line,New}|T]) ->
  if
    length(New) > length(Cur) -> convert_indents(Out ++ [{'begin',Line,New}], [New] ++ [Cur] ++ Indents, T)
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

convert({module, Name, Stmts}) ->
  Out=lists:flatten([ 
    io_lib:format("-module(~s).", [Name])
    ,{ line, 2 }
    , convert("", Stmts, [])
    ])
  ,convert_output("", 1, "", Out)
  ;
convert(List) when is_list(List) ->
  %expressions
  Out = convert_stmts(",", List, []) ++ "."
  ,convert_output("", 1, "", Out)
  .
  
convert_stmts(Stmts, State) ->
  convert_stmts("", Stmts, State)
  .

convert_stmts(Delimit, [], State) ->
  ""
  ;
convert_stmts(Delimit, Stmts, State) when not is_list(Stmts) ->
  convert("", [ Stmts ], State)
  ;
convert_stmts(Delimit, Stmts, State) when is_list(Delimit) ->
  convert_stmts({constant, nil, Delimit}, Stmts, State)
  ;
convert_stmts(Delimit, Stmts, State) ->
  convert("", list_insert(Delimit, Stmts), State)
  .

convert(Out, [], State) ->
  Out
  ;
convert(Out, [H|T], State) when element(1, H) == 'begin'; element(1, H) == 'end' ->
  convert(Out ++ convert2(H, nil, State), T, State)
  ;
convert(Out, [H,Peek|T], State) ->
  convert(Out ++ [ { line, element(2, H) } ] ++ convert2(H, Peek, State), [ Peek ] ++ T, State)
  ;
convert(Out, [H|T], State) ->
  convert(Out ++ [ { line, element(2, H) } ] ++ convert2(H, nil, State), T, State)
  .

convert2({export_stmt,_Line,Exports}, Next, State) ->
  "-export([" ++ convert_stmts(",", Exports, State) ++ "])."
  ;
convert2({export,_Line,Func,ArgCount}, Next, State) ->
  io_lib:format("~s/~p", [ Func, ArgCount ])
  ;
convert2({pre_author,_Line,Author}, Next, State) ->
  "-author(" ++ convert_stmts(Author, State) ++ ")."
  ;
convert2({pre_define,_Line,Term,Expr}, Next, State) ->
  "-define(" ++ convert_stmts(Term, State) ++ "," ++ convert_stmts(Expr, State) ++ ")."
  ;
convert2({'begin',Line,Indent}, Next, State) ->
  [ { indent, Indent}, { line, Line } ]
  ;
convert2({'end',Line,Indent}, Next, State) ->
  [ { indent, Indent} ]
  ;
convert2({constant,_Line,String}, Next, State) ->
  String
  ;
convert2({function_def,_Line,Name,Body}, Next, State) ->
  Term = case function_def_match({function_def,_Line,Name,Body}, Next) of
    true -> ";"
    ;false -> "."
    end
  ,Name ++ convert_stmts(Body, State) ++ Term
  ;
convert2({function_body, _, Args, When, Body}, Next, State) ->
  WhenPart = convert_stmts(When, State)
  ,convert_stmts(Args, State) ++ WhenPart ++ convert_stmts(",", Body, State)
  ;
convert2({'when', _Line, Guard}, Next, State) ->
  if
    Guard == nil -> " -> "
    ;true -> " when " ++ convert_stmts(Guard, State) ++ " -> "
  end
  ;
convert2({arg_list, _, Args}, Next, State) ->
  "(" ++ convert_stmts(", ", Args, State) ++ ")"
  ;
convert2({funccall, Line, Names, Args}, Next, State) ->
  convert_stmts(":", Names, State) ++ convert_stmts(Args, State)
  ;
convert2({'case', Line, Expr, Branches}, Next, State) ->
  "case " ++ convert_stmts(Expr, State) ++ " of " ++ convert_stmts("; ", Branches, State) ++ " end"
  ;
convert2({'if', Line, Branches}, Next, State) ->
  "if " ++ convert_stmts("; ", Branches, State) ++ " end"
  ;
convert2({'receive', Line, Branches}, Next, State) ->
  "receive " ++ convert_stmts("; ", Branches, State) ++ " end"
  ;
convert2({branch, _Line, Expr, When, Stmts}, Next, State) ->
  convert_stmts(Expr, State) ++ convert_stmts(When, State) ++ convert_stmts(", ", Stmts, State)
  ;
convert2({'after', Line, Expr, Stmts}, Next, State) ->
  "after " ++ convert_stmts(Expr, State) ++ " -> " ++ convert_stmts(", ", Stmts, State)
  ;
convert2({'fun', Line, Clauses}, Next, State) ->
  "fun " ++ convert_stmts("; ", Clauses, State) ++ " end"
  ;
convert2({'fun_export', Line, Export}, Next, State) ->
  "fun " ++ convert_stmts(Export, State)
  ;
convert2({'try', Line, Stmts, Catches, After}, Next, State) ->
  CatchPart = case Catches of
    nil -> ""
    ;_ -> " catch " ++ convert_stmts("; ", Catches, State)
  end
  ,AfterPart = case After of
    nil -> ""
    ;_ -> " after " ++ convert_stmts(", ", After, State)
  end
  ,"try " ++ convert_stmts(", ", Stmts, State) ++ CatchPart ++ AfterPart ++ " end"
  ;
convert2({macro, Line, Name, nil}, Next, State) ->
  Name
  ;
convert2({macro, Line, Name, Args}, Next, State) ->
  Name ++ convert_stmts(Args, State)
  ;
convert2({guard_expr, _Line, Expr}, Next, State) ->
  convert_stmts(Expr, [ guard_andor ] ++ State)
  ;
convert2({binary_op, _Line, "and", Left, Right}, Next, State) ->
  case proplists:is_defined(guard_andor, State) of
    true ->
      Without = proplists:delete(guard_andor, State)
      ,convert_stmts(Left, Without) ++ "," ++ convert_stmts(Right, Without)
    ;_ ->
      convert_stmts(Left, State) ++ " andalso " ++ convert_stmts(Right, State)
  end
  ;
convert2({binary_op, _Line, "or", Left, Right}, Next, State) ->
  case proplists:is_defined(guard_andor, State) of
    true ->
      Without = proplists:delete(guard_andor, State)
      ,convert_stmts(Left, Without) ++ ";" ++ convert_stmts(Right, Without)
    ;_ ->
      convert_stmts(Left, State) ++ " orelse " ++ convert_stmts(Right, State)
  end
  ;
convert2({binary_op, Line, Symbol, Left, Right}, Next, State) ->
  convert_stmts(Left, State) ++ " " ++ Symbol ++ " " ++ convert_stmts(Right, State)
  ;
convert2({unary_op, Line, Symbol, Right}, Next, State) ->
  Symbol ++ convert_stmts(Right, State)
  ;
convert2({ list, Line, Args, Tail }, Next, State) ->
  TailPart = case Tail of
    nil -> ""
    ;V -> "|" ++ convert_stmts(Tail, State)
    end
  ,"[" ++ convert_stmts(", ", Args, State) ++ TailPart ++ "]"
  ;
convert2({ tuple, Line, Args }, Next, State) ->
  "{" ++ convert_stmts(", ", Args, State) ++ "}"
  ;
convert2({ paren_expr, Line, Expr }, Next, State) ->
  "(" ++ convert_stmts(Expr, State) ++ ")"
  ;
convert2(H, Next, State) ->
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
  [H] ++ list_insert(Symbol, List)
  ;
list_insert(Symbol, List) ->
  case List of
    [H|_] when element(1, H) == 'end'; element(1, H) == 'after' ->
      list_insert([], Symbol, List)
    ;_ ->
      [_|Result] = list_insert([], Symbol, List)
      ,Result
  end
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
  {module,J} = load_file("examples/example", [debug_info,verbose,report])
  ,6=J:fac(3)
  %Lookup eunit docs for more...
  .
