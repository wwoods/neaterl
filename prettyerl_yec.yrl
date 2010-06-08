%Pretty erlang grammar

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
func_def_body
arg_list arg_parts
guard_expression
expression expression_atom uminus
list tuple
func_call func_call_name
anon_fun anon_fun_clause_block anon_fun_clause_line anon_fun_clause_list anon_fun_clause
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.' '>' '<'
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

module -> prep_module atom ')' line export line module_statement_list
  : { module, value_of('$2'), '$5', '$7' }.
export -> prep_export '[' ']' ')'
  : [].
export -> prep_export '[' export_list ']' ')'
  : '$3'.
export_list -> export_func : [ '$1' ].
export_list -> export_func sep export_list
  : [ '$1' ] ++ '$3' .
export_func -> atom '/' integer : { export, line_of('$1'), value_of('$1'), value_of('$3') }.
  
module_statement_list -> module_statement : stmts_to_list('$1').
module_statement_list -> module_statement line module_statement_list : stmts_to_list('$1') ++ '$3'.
module_statement_list -> line : [].

module_statement -> atom func_def_body : { function_def, line_of('$1'), value_of('$1'), '$2' }.
module_statement -> preproc : { constant, line_of('$1'), list_value_of('$1') ++ "." }.

statement_block -> statement_line : '$1'.
statement_block -> 'begin' statement_list 'end' : [ '$1' ] ++ '$2'.

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
expression -> expression '!' expression : { 'send', line_of('$1'), '$1', '$3' }.
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
uminus -> '-' expression : { unary_op, line_of('$1'), "-", '$2' }.

expression_atom -> atom : constant_from('$1').
expression_atom -> macro : { macro, line_of('$1'), list_value_of('$1'), nil }.
expression_atom -> macro '(' ')' : { macro, line_of('$1'), list_value_of('$1'), [] }.
expression_atom -> macro '(' arg_list ')' : { constant, line_of('$1'), list_value_of('$1'), '$3' }.
expression_atom -> variable : constant_from('$1').
expression_atom -> integer : constant_from('$1').
expression_atom -> float : constant_from('$1').
expression_atom -> string : constant_from('$1').
expression_atom -> '(' expression ')' : '$2'.

branch_block -> 'begin' branch_list 'end' : [ '$1' ] ++ '$2'.
branch_block -> branch_line : '$1'.

branch_list -> branch : [ '$1' ].
branch_list -> branch line branch_list : [ '$1' ] ++ '$3'.

branch_line -> branch : [ '$1' ].
branch_line -> branch branch_line : [ '$1' ] ++ '$2'.

branch -> guard_expression '->' statement_block : { branch, line_of('$1'), '$1', '$3' }.
branch -> 'after' expression '->' statement_block : { 'after', line_of('$1'), '$2', '$4' }.

anon_fun -> 'fun' anon_fun_clause_block : { 'fun', line_of('$1'), '$2' }.

anon_fun_clause_block -> anon_fun_clause_line : '$1'.
anon_fun_clause_block -> 'begin' anon_fun_clause_list 'end' : [ '$1' ] ++ '$2'.

anon_fun_clause_line -> anon_fun_clause : [ '$1' ].
anon_fun_clause_line -> anon_fun_clause anon_fun_clause_line : [ '$1' ] ++ '$2'.

anon_fun_clause_list -> anon_fun_clause : [ '$1' ].
anon_fun_clause_list -> anon_fun_clause line anon_fun_clause_list : [ '$1' ] ++ '$3'.

anon_fun_clause -> func_def_body : '$1'.

func_call -> expression_atom arg_list : { funccall, line_of('$1'), [ '$1' ], '$2' }.
func_call -> expression_atom ':' func_call : { funccall, line_of('$1'), [ '$1' ] ++ element(3, '$3'), element(4, '$3') }.

arg_list -> '(' ')' : [].
arg_list -> '(' arg_parts ')' : '$2'.
arg_parts -> expression : [ '$1' ].
arg_parts -> expression sep arg_parts : [ '$1' ] ++ '$3'.

func_def_body -> arg_list '->' statement_block : { function_body, nil, '$1', nil, '$3' }.
func_def_body -> arg_list 'when' guard_expression '->' statement_block : { function_body, nil, '$1', '$3', '$5' }.

%seps is any statement separator (line breaks or ',')
sep -> line : nil.
sep -> ',' : nil.

list -> '[' ']' : { list, [] }.
list -> '[' arg_parts ']' : { list, line_of('$1'), '$2' }.

tuple -> '{' '}' : { tuple, [] }.
tuple -> '{' arg_parts '}' : { tuple, line_of('$1'), '$2' }.

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
