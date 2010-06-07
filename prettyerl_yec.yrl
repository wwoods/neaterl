%Pretty erlang grammar

%Differences
%Commas and unescaped newlines are both separators

%Grammar notes:
%When a list is expected, suffix it with _list, and support
%the empty case when applicable.

Nonterminals 
module export export_list export_list2 export_func
statement_list statement
seps sep indents
function_def 
arg_list arg_list2
guard_expression
expression expression_atom uminus
list tuple
func_call func_call_name
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.' '>' '<'
'->' '++' '--' '!' ':' ';' '=' '==' '>=' '<='
indent ','
atom float integer variable string macro
prep_module prep_export preproc
'case' 'of' 'if' 'end' 'when'
'andalso' 'orelse' 'fun' 'not'
'receive' 'after'
.

Rootsymbol module.

Left 5 ';'.
Left 10 ','.
Left 15 '!'.
Left 20 '='.
Left 20 '=='.
Left 20 '>'.
Left 20 '<'.
Left 50 '++'.
Left 50 '--'.
Left 100 '+'.
Left 100 '-'.
Left 200 '*'.
Left 200 '/'.
Unary 300 uminus.

module -> prep_module atom ')' seps export statement_list
  : { module, value_of('$2'), '$5', '$6' }.
export -> prep_export '[' ']' ')'
  : [].
export -> prep_export '[' export_list ']' ')'
  : '$3'.
export_func -> atom '/' integer : { export, value_of('$1'), value_of('$3') }.
export_list -> seps export_list2 : '$2'.
export_list -> export_list2 : '$1'.
export_list2 -> export_func : [ '$1' ].
export_list2 -> export_func seps export_list2 
  : [ '$1' ] ++ '$3' .

statement_list -> indents : [].
statement_list -> indents statement : [ { '$1', stmts_to_list('$2') } ].
statement_list -> indents statement statement_list : [ { '$1', stmts_to_list('$2') } ] ++ '$3'.

statement -> statement ',' statement : ['$1', '$3'].
statement -> function_def : '$1'.
statement -> function_def statement : ['$1', '$2'].
statement -> expression '->' : { branch_condition, '$1' }.
statement -> expression '->' statement : [ { branch_condition, '$1' }, '$3' ].
statement -> 'after' expression '->' : { 'after', '$2', [] }.
statement -> 'after' expression '->' statement : { 'after', '$2', [ '$4' ] }.
statement -> expression : '$1'.
statement -> preproc : { constant, list_value_of('$1') ++ "." }.

function_def -> atom '(' ')' '->' : { function_def, value_of('$1'), [], nil }.
function_def -> atom '(' ')' 'when' guard_expression '->' : { function_def, value_of('$1'), [], '$5' }.
function_def -> atom '(' arg_list ')' '->' : { function_def, value_of('$1'), '$3', nil }.
function_def -> atom '(' arg_list ')' 'when' guard_expression '->' 
  : { function_def, value_of('$1'), '$3', '$6' }.

arg_list -> arg_list2 : '$1'.
arg_list -> seps arg_list2 : '$2'.
arg_list2 -> expression : [ '$1' ].
arg_list2 -> expression seps arg_list2 : [ '$1' ] ++ '$3'.

guard_expression -> expression : '$1'.
guard_expression -> guard_expression ',' guard_expression : { binary_op, ",", '$1', '$3' }.

%seps is any separator (indent or ',')
seps -> sep : nil.
seps -> sep seps : nil.
sep -> indent : nil.
sep -> ',' : nil.

%line is any number of indents
indents -> indent : '$1'.
indents -> indent indents : '$2'.

%Statements that are actually expressions...
expression -> expression '!' expression : { 'send', '$1', '$3' }.
expression -> 'case' expression 'of' : { 'case', '$2' }.
expression -> 'if' : { 'if' }.
expression -> 'receive' : { 'receive', [] }.
expression -> 'receive' statement : { 'receive', [ '$2' ] }.

expression_atom -> atom : { constant, list_value_of('$1') }.
expression_atom -> macro : { macro, list_value_of('$1'), nil }.
expression_atom -> macro '(' ')' : { macro, list_value_of('$1'), [] }.
expression_atom -> macro '(' arg_list ')' : { constant, list_value_of('$1'), '$3' }.
expression_atom -> variable : { constant, list_value_of('$1') }.
expression_atom -> integer : { constant, list_value_of('$1') }.
expression_atom -> float : { constant, list_value_of('$1') }.
expression_atom -> string : { constant, list_value_of('$1') }.

expression -> expression_atom : '$1'.
expression -> func_call : '$1'.
expression -> list : '$1'.
expression -> tuple : '$1'.
expression -> expression '>' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '<' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '>=' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '<=' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '==' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '=' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '+' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '-' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '*' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '/' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '++' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> expression '--' expression : { binary_op, list_value_of('$2'), '$1', '$3' }.
expression -> uminus : '$1'.
expression -> '(' expression ')' : '$2'.
uminus -> '-' expression : { unary_op, "-", '$2' }.

%Weird bug here, can't replace atom with expression_atom, which sucks.
func_call -> atom '(' ')' : { funccall, [ { constant, list_value_of('$1') } ], [] }.
func_call -> atom '(' arg_list ')' : { funccall, [ { constant, list_value_of('$1') } ], '$3' }.
func_call -> variable '(' ')' : { funccall, [ { constant, list_value_of('$1') } ], [] }.
func_call -> variable '(' arg_list ')' : { funccall, [ { constant, list_value_of('$1') } ], '$3' }.
func_call -> '(' expression ')' '(' ')' : { funccall, ['$2'], [] }.
func_call -> '(' expression ')' '(' arg_list ')' : { funccall, ['$2'], '$5' }.
func_call -> expression_atom ':' func_call 
  : { funccall, Names, Args } = '$3'
  , { funccall, ['$1'] ++ Names, Args }
  .
func_call -> '(' expression ')' ':' func_call 
  : { funccall, Names, Args } = '$5'
  , { funccall, ['$2'] ++ Names, Args }
  .

list -> '[' ']' : { list, [] }.
list -> '[' arg_list ']' : { list, '$2' }.

tuple -> '{' '}' : { tuple, [] }.
tuple -> '{' arg_list '}' : { tuple, '$2' }.

Erlang code.
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
