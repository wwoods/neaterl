%Pretty erlang grammar

%Differences
%Commas and unescaped newlines are both separators

%Grammar notes:
%When a list is expected, suffix it with _list, and support
%the empty case when applicable.

Nonterminals 
module export export_list export_list2 export_func
module_statement_list module_statement
seps sep
statlist element elements
.

Terminals 
'(' ')' '@' '[' ']' '{' '}' '+' '-' '/' '*' '.'
separator
atom float integer variable 
prep_module prep_export 
'case' 'if' 'end'
.

Rootsymbol module.

module -> prep_module '(' atom ')' seps export module_statement_list
  : { module, value_of('$3'), '$6', '$7' }.
export -> prep_export '(' '[' ']' ')'
  : [].
export -> prep_export '(' '[' export_list ']' ')'
  : '$4'.
export_func -> atom '/' integer : { export, value_of('$1'), value_of('$3') }.
export_list -> seps export_list2 : '$2'.
export_list -> export_list2 : '$1'.
export_list2 -> export_func : [ '$1' ].
export_list2 -> export_func seps export_list2 : { lists, [ '$1' ], '$3' }.

module_statement_list -> seps module_statement : '$2'.
module_statement_list -> seps module_statement module_statement_list : { stmt_list, '$2', '$3' }.

module_statement -> atom : '$1'.

seps -> sep : nil.
seps -> sep seps : nil.
sep -> separator : nil.

%list -> '[' ']' : {list,[],[]}.
%list -> '[' elements ']' : '$2'.

%elements -> element : {cons, '$1', nil}.
%elements -> element '[' elements : { cons, '$1', '$3'}.
%element -> atom : '$1'.
%element -> list : '$1'.

Erlang code.
value_of(Token) ->
  element(3, Token).
