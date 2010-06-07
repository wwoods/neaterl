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
expression uminus
list
func_call
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.' '>' '<'
'->' '++' '--' '!' ':' ';' '=' '==' '>=' '<='
indent ','
atom float integer variable string
prep_module prep_export 
'case' 'if' 'end' 'when'
'andalso' 'orelse' 'fun' 'not'
.

Rootsymbol module.

Left 5 ';'.
Left 10 ','.
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

module -> prep_module '(' atom ')' seps export statement_list
  : { module, value_of('$3'), '$6', '$7' }.
export -> prep_export '(' '[' ']' ')'
  : [].
export -> prep_export '(' '[' export_list ']' ')'
  : '$4'.
export_func -> atom '/' integer : { export, value_of('$1'), value_of('$3') }.
export_list -> seps export_list2 : '$2'.
export_list -> export_list2 : '$1'.
export_list2 -> export_func : [ '$1' ].
export_list2 -> export_func seps export_list2 
  : [ '$1' ] ++ '$3' .

statement_list -> indents statement : [ { '$1', stmts_to_list('$2') } ].
statement_list -> indents statement statement_list : [ { '$1', stmts_to_list('$2') } ] ++ '$3'.

statement -> statement ',' statement : ['$1', '$3'].
statement -> function_def : '$1'.
statement -> function_def statement : ['$1', '$2'].
statement -> expression : '$1'.

function_def -> atom '(' ')' '->' : { function_def, value_of('$1'), [], nil }.
function_def -> atom '(' arg_list ')' '->' : { function_def, value_of('$1'), '$3', nil }.
function_def -> atom '(' ')' 'when' guard_expression '->' : { function_def, value_of('$1'), [], '$5' }.
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

expression -> atom : { constant, list_value_of('$1') }.
expression -> list : '$1'.
expression -> variable : { constant, list_value_of('$1') }.
expression -> integer : { constant, list_value_of('$1') }.
expression -> float : { constant, list_value_of('$1') }.
expression -> string : { constant, list_value_of('$1') }.
expression -> func_call : '$1'.
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

func_call -> atom ':' atom '(' ')' : { funccall
  , line_of('$1')
  , list_value_of('$1') ++ ":" ++ list_value_of('$3')
  , [] 
  }.
func_call -> atom ':' atom '(' arg_list ')' : { funccall
  , line_of('$1')
  , list_value_of('$1') ++ ":" ++ list_value_of('$3')
  , '$5' 
  }.
func_call -> atom '(' ')' : { funccall, line_of('$1'), list_value_of('$1'), [] }.
func_call -> atom '(' arg_list ')' : { funccall, line_of('$1'), list_value_of('$1'), '$3' }.

list -> '[' ']' : { list, [] }.
list -> '[' arg_list ']' : { list, '$2' }.

Erlang code.
value_of(Token) ->
  element(3, Token).
line_of(Token) ->
  element(2, Token).
ensure_list(Var) ->
  if is_list(Var) -> Var
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
