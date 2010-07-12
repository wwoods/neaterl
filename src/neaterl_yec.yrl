%Neat / tidy erlang grammar

%TODO - Binary Support
%TODO - Record support
%TODO - Change catch syntax from "Type of Reason" (maybe)
%TODO - Capitalize Nonterminals.
%TODO - Multiline comments / remainder / div

%Differences
%Commas and unescaped newlines are both separators

%Grammar notes:
%When a list is expected, suffix it with _list, and support
%the empty case when applicable.

%; for inline block ends (any of the _line non terminals)

Nonterminals 
sep
module module_start export export_list export_func
module_statement_list module_statement
statement_list statement_block statement_line 
statement_with_line statement_with_noline
branch_block branch_list branch_line branch
func_def_body when_clause
arg_list arg_parts_inline arg_parts_list arg_parts_list2
guard_expression
expression expression_atom uminus unot ucatch binary_op try_expr
list tuple
list_arg_parts_list list_arg_parts_list2
func_call 
anon_fun fun_clause_block fun_clause_line fun_clause_list
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.' '>' '<' '|' '#' 'div' 'rem'
'->' '++' '--' '!' ':' ';' '=' '==' '>=' '=<'
'/=' '=:=' '=/='
line 'begin' 'end' ',' 'char_expr'
atom float integer variable string macro
prep_module prep_export prep_import prep_author prep_define
prep_behaviour prep_compile prep_extends
'case' 'of' 'if' 'when'
'andalso' 'orelse' 'not' 'and' 'or' 'xor'
'fun'
'receive' 'after'
'try' 'catch'
.

Rootsymbol module_start.

% Intentionally reordered.. though not entirely effective at the moment, 
% since it compiles down to erlang :)
Left 5 ';'.
Left 10 ','.
Nonassoc 12 '->'.
Right 15 '!'.
Right 20 '='.
Left 30 'orelse'.
Left 31 'andalso'.
Left 40 'and'.
Left 40 'or'.
Left 40 'xor'.
Unary 50 unot.
Unary 50 ucatch.
Left 60 '=='.
Left 60 '=:='.
Left 60 '/='.
Left 60 '=/='.
Left 60 '>'.
Left 60 '<'.
Left 60 '>='.
Left 60 '=<'.
Right 80 '++'.
Right 80 '--'.
Left 100 '+'.
Left 100 '-'.
Left 200 '*'.
Left 200 '/'.
Left 200 'div'.
Left 200 'rem'.
%'rem' and 'div' at some point - integer remainding and division.  Maybe under % and / though.
Unary 800 uminus.
Left 900 '#'.
Left 1000 ':'.

module_start -> module : '$1'.
module_start -> line module : '$2'.

module -> prep_module '(' atom ')' line module_statement_list
  : { module, value_of('$3'), '$6' }.
  
%This is a hack... if the first line is not a module declaration, parse a
%statement list instead
module -> statement_list : '$1'.
  
export -> prep_export '(' '[' ']' ')'
  : { export_stmt, line_of('$1'), [] }.
export -> prep_export '(' '[' export_list ']' ')'
  : { export_stmt, line_of('$1'), '$4' }.
export_list -> export_func : [ '$1' ].
export_list -> export_func sep export_list
  : [ '$1' ] ++ '$3' .
export_func -> atom '/' integer : { export, line_of('$1'), value_of('$1'), value_of('$3') }.
  
module_statement_list -> module_statement line : stmts_to_list('$1').
module_statement_list -> module_statement line module_statement_list : stmts_to_list('$1') ++ '$3'.

module_statement -> atom func_def_body : { function_def, line_of('$1'), list_value_of('$1'), '$2' }.
module_statement -> atom '/' integer fun_clause_block : { function_def, line_of('$1'), list_value_of('$1'), '$4' }.
module_statement -> export : '$1'.
module_statement -> prep_import '(' atom ')' : { constant, line_of('$1'), "-import(" ++ list_value_of('$3') ++ ")." }.
module_statement -> prep_author '(' expression ')' : { pre_author, line_of('$1'), '$3' }.
module_statement -> prep_behaviour '(' atom ')' : { constant, line_of('$1'), "-behaviour(" ++ list_value_of('$3') ++ ")." }.
module_statement -> prep_extends '(' atom ')' : { constant, line_of('$1'), "-extends(" ++ list_value_of('$3') ++ ")." }.
module_statement -> prep_compile '(' expression ')' : { pre_compile, line_of('$1'), '$3' }.
module_statement -> prep_define '(' expression ',' statement_line ')' : { pre_define, line_of('$1'), '$3', '$5' }.
module_statement -> prep_define '(' 'begin' expression line statement_list 'end' : { pre_define, line_of('$1'), '$4', '$6' }.

statement_block -> statement_line : '$1'.
statement_block -> 'begin' statement_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].

statement_list -> statement_with_line : [ '$1' ].
statement_list -> statement_with_line statement_list : [ '$1' ] ++ '$2'.

statement_line -> statement_with_noline : stmts_to_list('$1').
%statement_line -> statement ',' statement_line : stmts_to_list('$1') ++ '$3'.
%statement_line -> statement ';' : stmts_to_list('$1').

statement_with_noline -> expression : '$1'.

statement_with_line -> expression line : '$1'.
statement_with_line -> try_expr : '$1'.

guard_expression -> expression : { guard_expr, line_of('$1'), '$1' }.

%Remember, some statements are actually expressions...
expression -> 'case' expression 'of' branch_block : { 'case', line_of('$1'), '$2', '$4' }.
expression -> 'if' branch_block : { 'if', line_of('$1'), '$2' }.
expression -> 'receive' branch_block : { 'receive', line_of('$1'), '$2' }.
expression -> func_call : '$1'.
expression -> anon_fun : '$1'.
expression -> expression binary_op expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression_atom : '$1'.
expression -> uminus : '$1'.
expression -> unot : '$1'.
expression -> ucatch : '$1'.
uminus -> '-' expression : { unary_op, line_of('$1'), "-", '$2' }.
unot -> 'not' expression : { unary_op, line_of('$1'), "not ", '$2' }.
ucatch -> 'catch' expression : { unary_op, line_of('$1'), "catch ", '$2' }.

%It's probably an LALR(1) restriction that there can't be an optional 
%line before the operator, but I'd like to be able to insert one.
binary_op -> '!' : '$1'.
binary_op -> 'and' : '$1'.
binary_op -> 'or' : '$1'.
binary_op -> 'xor' : '$1'.
binary_op -> '>' : '$1'.
binary_op -> '<' : '$1'.
binary_op -> '>=' : '$1'.
binary_op -> '=<' : '$1'.
binary_op -> '==' : '$1'.
binary_op -> '/=' : '$1'.
binary_op -> '=:=' : '$1'.
binary_op -> '=/=' : '$1'.
binary_op -> '=' : '$1'.
binary_op -> '+' : '$1'.
binary_op -> '-' : '$1'.
binary_op -> '*' : '$1'.
binary_op -> '/' : '$1'.
binary_op -> 'div' : { 'div', line_of('$1'), " div " }.
binary_op -> 'rem' : { 'rem', line_of('$1'), " rem " }.
binary_op -> '++' : '$1'.
binary_op -> '--' : '$1'.

expression_atom -> atom : constant_from('$1').
expression_atom -> macro : { macro, line_of('$1'), list_value_of('$1'), nil }.
expression_atom -> macro arg_list : { macro, line_of('$1'), list_value_of('$1'), '$2' }.
expression_atom -> variable : constant_from('$1').
expression_atom -> integer : constant_from('$1').
expression_atom -> float : constant_from('$1').
expression_atom -> string : constant_from('$1').
expression_atom -> 'char_expr' : constant_from('$1').
expression_atom -> list : '$1'.
expression_atom -> tuple : '$1'.
expression_atom -> '(' expression ')' : { paren_expr, line_of('$1'), '$2' }.

branch_block -> 'begin' branch_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].
branch_block -> branch_line : '$1'.

branch_list -> branch line : [ '$1' ].
branch_list -> branch line branch_list : [ '$1' ] ++ '$3'.

branch_line -> branch : [ '$1' ].
%branch_line -> branch branch_line : [ '$1' ] ++ '$2'.

branch -> expression when_clause statement_block : { branch, line_of('$1'), '$1', '$2', '$3' }.
branch -> 'after' expression '->' statement_block : { 'after', line_of('$1'), '$2', '$4' }.
branch -> expression 'of' expression when_clause statement_block : { branch, line_of('$1'), { binary_op, line_of('$1'), ":", '$1', '$3' }, '$4', '$5' }.

anon_fun -> 'fun' fun_clause_block : { 'fun', line_of('$1'), '$2' }.
anon_fun -> 'fun' export_func : { 'fun_export', line_of('$1'), '$2' }.

fun_clause_block -> fun_clause_line : '$1'.
fun_clause_block -> 'begin' fun_clause_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].

fun_clause_line -> func_def_body : [ '$1' ].
fun_clause_line -> func_def_body ',' fun_clause_line : [ '$1' ] ++ '$3'.

fun_clause_list -> func_def_body line : [ '$1' ].
fun_clause_list -> func_def_body line fun_clause_list : [ '$1' ] ++ '$3'.

%try_expr is a statement because it uses multiple lines...Consider using it as an expression
%in a parameter:
%myfunc(
%  parm1
%  try parm2
%  catch c
%  parm3
%For this, the catch() expression should be used instead.
try_expr -> 'try' statement_block line 'catch' branch_block line : { 'try', line_of('$1'), '$2', '$5', nil }.
try_expr -> 'try' statement_block line 'catch' branch_block line 'after' statement_block line : { 'try', line_of('$1'), '$2', '$5', '$8' }.
try_expr -> 'try' statement_block line 'after' statement_block line : { 'try', line_of('$1'), '$2', nil, '$5' }.
% Excluding try..of unless I hear a good reason it's meaningful.
% I could see scoping arguments, maybe.  But they don't appear to be affected.

%WHY this is a statement - it has two lines.
%Consider:
%try hello
%catch
%  blah:blah -> blah

%try hello catch
%  blah:blah -> blah

%try
%  hello
%catch
%  blah:blah -> blah

%How that works in params??
%my_func(
%  try hello
%  catch goodbye
%  parm2
%  parm3
%ewww...

%my_func(
%  try hello
%    catch goodbye
%  parm2
%  parm3

%my_func(
%  try
%    a
%    b
%    c
%    catch
%      123

func_call -> expression_atom arg_list : { funccall, line_of('$1'), [ '$1' ], '$2' }.
func_call -> expression_atom ':' func_call : { funccall, line_of('$1'), [ '$1' ] ++ element(3, '$3'), element(4, '$3') }.

arg_list -> '(' ')' : { arg_list, line_of('$1'), [] }.
arg_list -> '(' arg_parts_inline ')' : { arg_list, line_of('$1'), '$2' }.
arg_list -> '(' arg_parts_list : { arg_list, line_of('$1'), '$2' }.

arg_parts_inline -> expression : [ '$1' ].
arg_parts_inline -> expression ',' arg_parts_inline : [ '$1' ] ++ '$3'.

arg_parts_list -> 'begin' arg_parts_list2 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].
arg_parts_list2 -> expression line : [ '$1' ].
arg_parts_list2 -> expression line arg_parts_list2 : [ '$1' ] ++ '$3'.

func_def_body -> arg_list when_clause statement_block : { function_body, line_of('$1'), '$1', '$2', '$3' }.
func_def_body -> tuple when_clause statement_block : { function_body, line_of('$1'), { arg_list, line_of('$1'), [ '$1' ] }, '$2', '$3' }.

when_clause -> '->' : { 'when', line_of('$1'), nil }.
when_clause -> 'when' guard_expression '->' : { 'when', line_of('$1'), '$2' }.
when_clause -> line 'when' guard_expression line '->' : { 'when', line_of('$2'), '$3' }.

%seps is any statement separator (line breaks or ',')
sep -> line : nil.
sep -> ',' : nil.

list -> '[' ']' : { list, line_of('$1'), [], nil }.
list -> '[' arg_parts_inline ']' : { list, line_of('$1'), '$2', nil }.
list -> '[' arg_parts_inline '|' expression_atom ']' : { list, line_of('$1'), '$2', '$4' }.
list -> '[' list_arg_parts_list :
  case '$2' of
    {X,Y} -> { list, line_of('$1'), X, Y }
    ;Z -> { list, line_of('$1'), Z, nil }
  end
  .

list_arg_parts_list -> 'begin' list_arg_parts_list2 'end' 
  : 
    case lists:last('$2') of
      { list_tail, X } -> { [ '$1' ] ++ lists:sublist('$2', length('$2') - 1) ++ [ '$3' ], X }
      ;_ -> [ '$1' ] ++ '$2' ++ [ '$3' ]
    end
  .

list_arg_parts_list2 -> expression line: [ '$1' ].
list_arg_parts_list2 -> expression line list_arg_parts_list2 : [ '$1' ] ++ '$3'.
list_arg_parts_list2 -> '|' expression line : [ { list_tail, '$2' } ].

tuple -> '{' '}' : { tuple, line_of('$1'), [] }.
tuple -> '{' arg_parts_inline '}' : { tuple, line_of('$1'), '$2' }.
tuple -> '{' arg_parts_list : { tuple, line_of('$1'), '$2' }.

Erlang code.
constant_from({_,Line,Value}) ->
  { constant, Line, list_value_of2(Value) }.
value_of(Token) ->
  element(3, Token).
line_of(Token) ->
  element(2, Token).
ensure_list(Var) ->
  if is_list(Var) -> lists:flatten(Var)
  ; true -> [ Var ]
  end
  .
stmts_to_list([]) -> []
;stmts_to_list([H|T]) ->
  ensure_list(H) ++ stmts_to_list(T)
;stmts_to_list(Other) ->
  [Other]
  .

list_value_of(N) -> list_value_of2(element(3, N)).

list_value_of2(N) when is_atom(N) -> atom_to_list(N)
;list_value_of2(N) when is_integer(N) -> integer_to_list(N)
;list_value_of2(N) when is_float(N) -> float_to_list(N)
;list_value_of2(N) when is_list(N) -> N
.
