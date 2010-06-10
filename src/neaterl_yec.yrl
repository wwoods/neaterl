%Neat / tidy erlang grammar

%TODO - Capitalize Nonterminals.

%Differences
%Commas and unescaped newlines are both separators

%Grammar notes:
%When a list is expected, suffix it with _list, and support
%the empty case when applicable.

%; for inline block ends (any of the _line non terminals)

Nonterminals 
sep
module export export_list export_func
module_statement_list module_statement
statement_list statement_block statement_line statement
branch_block branch_list branch_line branch
func_def_body func_def_when
arg_list arg_parts_inline arg_parts_list arg_parts_list2
guard_expression
expression expression_atom uminus unot
list tuple
list_arg_parts_list list_arg_parts_list2
func_call 
anon_fun anon_fun_clause_block anon_fun_clause_line anon_fun_clause_list anon_fun_clause
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.' '>' '<' '|'
'->' '++' '--' '!' ':' ';' '=' '==' '>=' '<='
line 'begin' 'end' ','
atom float integer variable string macro
prep_module prep_export preproc
'case' 'of' 'if' 'when'
'andalso' 'orelse' 'fun' 'not'
'receive' 'after'
.

Rootsymbol module.

Left 5 ';'.
Left 10 ','.
Nonassoc 12 '->'.
Left 13 'orelse'.
Left 14 'andalso'.
Left 15 '!'.
Left 20 '='.
Left 20 '=='.
Left 20 '>'.
Left 20 '<'.
Left 20 '>='.
Left 20 '<='.
Left 50 '++'.
Left 50 '--'.
Left 100 '+'.
Left 100 '-'.
Left 200 '*'.
Left 200 '/'.
Unary 300 uminus.
Unary 300 unot.

module -> prep_module '(' atom ')' line export line module_statement_list
  : { module, value_of('$3'), '$6', '$8' }.
export -> prep_export '(' '[' ']' ')'
  : [].
export -> prep_export '(' '[' export_list ']' ')'
  : '$4'.
export_list -> export_func : [ '$1' ].
export_list -> export_func sep export_list
  : [ '$1' ] ++ '$3' .
export_func -> atom '/' integer : { export, line_of('$1'), value_of('$1'), value_of('$3') }.
  
module_statement_list -> module_statement : stmts_to_list('$1').
module_statement_list -> module_statement line module_statement_list : stmts_to_list('$1') ++ '$3'.
module_statement_list -> line : [].

module_statement -> atom func_def_body : { function_def, line_of('$1'), list_value_of('$1'), '$2' }.
module_statement -> preproc : { constant, line_of('$1'), list_value_of('$1') ++ "." }.

statement_block -> statement_line : '$1'.
statement_block -> 'begin' statement_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].

statement_list -> statement_block : '$1'.
statement_list -> statement_block line statement_list : '$1' ++ '$3'.

statement_line -> statement : stmts_to_list('$1').
statement_line -> statement ',' statement_line : stmts_to_list('$1') ++ '$3'.
statement_line -> statement ';' : stmts_to_list('$1').

statement -> expression : '$1'.

guard_expression -> expression : '$1'.
guard_expression -> guard_expression ',' guard_expression : { binary_op, line_of('$1'), ",", '$1', '$3' }.
guard_expression -> guard_expression ';' guard_expression : { binary_op, line_of('$1'), ";", '$1', '$3' }.

%Remember, some statements are actually expressions...
expression -> 'case' expression 'of' branch_block : { 'case', line_of('$1'), '$2', '$4' }.
expression -> 'if' branch_block : { 'if', line_of('$1'), '$2' }.
expression -> 'receive' branch_block : { 'receive', line_of('$1'), '$2' }.
expression -> func_call : '$1'.
expression -> anon_fun : '$1'.
expression -> list : '$1'.
expression -> tuple : '$1'.
expression -> expression_atom : '$1'.
expression -> expression '!' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression 'andalso' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression 'orelse' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '>' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '<' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '>=' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '<=' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '==' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '=' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '+' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '-' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '*' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '/' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '++' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> expression '--' expression : { binary_op, line_of('$1'), list_value_of('$2'), '$1', '$3' }.
expression -> uminus : '$1'.
expression -> unot : '$1'.
uminus -> '-' expression : { unary_op, line_of('$1'), "-", '$2' }.
unot -> 'not' expression : { unary_op, line_of('$1'), "not ", '$2' }.

expression_atom -> atom : constant_from('$1').
expression_atom -> macro : { macro, line_of('$1'), list_value_of('$1'), nil }.
expression_atom -> macro arg_list : { macro, line_of('$1'), list_value_of('$1'), '$2' }.
expression_atom -> variable : constant_from('$1').
expression_atom -> integer : constant_from('$1').
expression_atom -> float : constant_from('$1').
expression_atom -> string : constant_from('$1').
expression_atom -> '(' expression ')' : { paren_expr, line_of('$1'), '$2' }.

branch_block -> 'begin' branch_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].
branch_block -> branch_line : '$1'.

branch_list -> branch : [ '$1' ].
branch_list -> branch line branch_list : [ '$1' ] ++ '$3'.

branch_line -> branch : [ '$1' ].
branch_line -> branch branch_line : [ '$1' ] ++ '$2'.

branch -> guard_expression '->' statement_block : { branch, line_of('$1'), '$1', '$3' }.
branch -> 'after' expression '->' statement_block : { 'after', line_of('$1'), '$2', '$4' }.

anon_fun -> 'fun' anon_fun_clause_block : { 'fun', line_of('$1'), '$2' }.

anon_fun_clause_block -> anon_fun_clause_line : '$1'.
anon_fun_clause_block -> 'begin' anon_fun_clause_list 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].

anon_fun_clause_line -> anon_fun_clause : [ '$1' ].
anon_fun_clause_line -> anon_fun_clause anon_fun_clause_line : [ '$1' ] ++ '$2'.

anon_fun_clause_list -> anon_fun_clause : [ '$1' ].
anon_fun_clause_list -> anon_fun_clause line anon_fun_clause_list : [ '$1' ] ++ '$3'.

anon_fun_clause -> func_def_body : '$1'.

func_call -> expression_atom arg_list : { funccall, line_of('$1'), [ '$1' ], '$2' }.
func_call -> expression_atom ':' func_call : { funccall, line_of('$1'), [ '$1' ] ++ element(3, '$3'), element(4, '$3') }.

arg_list -> '(' ')' : { arg_list, line_of('$1'), [] }.
arg_list -> '(' arg_parts_inline ')' : { arg_list, line_of('$1'), '$2' }.
arg_list -> '(' arg_parts_list : { arg_list, line_of('$1'), '$2' }.

arg_parts_inline -> expression : [ '$1' ].
arg_parts_inline -> expression sep arg_parts_inline : [ '$1' ] ++ '$3'.

arg_parts_list -> 'begin' arg_parts_list2 'end' : [ '$1' ] ++ '$2' ++ [ '$3' ].
arg_parts_list2 -> expression : [ '$1' ].
arg_parts_list2 -> expression line arg_parts_list2 : [ '$1' ] ++ '$3'.

func_def_body -> arg_list func_def_when statement_block : { function_body, nil, '$1', '$2', '$3' }.
func_def_body -> arg_list line func_def_when statement_block : { function_body, nil, '$1', '$3', '$4' }.

func_def_when -> '->' : nil.
func_def_when -> 'when' guard_expression '->' : '$2'.
func_def_when -> 'when' guard_expression line '->' : '$2'.

%seps is any statement separator (line breaks or ',')
sep -> line : nil.
sep -> ',' : nil.

list -> '[' ']' : { list, line_of('$1'), [], nil }.
list -> '[' arg_parts_inline ']' : { list, line_of('$1'), '$2', nil }.
list -> '[' arg_parts_inline '|' variable ']' : { list, line_of('$1'), '$2', constant_from('$4') }.
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

list_arg_parts_list2 -> expression : [ '$1' ].
list_arg_parts_list2 -> expression line list_arg_parts_list2 : [ '$1' ] ++ '$3'.
list_arg_parts_list2 -> '|' expression : [ { list_tail, '$2' } ].

tuple -> '{' '}' : { tuple, [] }.
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
