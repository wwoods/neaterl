
Definitions.

D = [0-9]

Rules.

\\\n :
  skip_token.
\%[^\n]* :
  skip_token.
\n\-[a-z] :
  %Beginning of preprocessor, make an indent record and
  %push back the dash
  {token,{indent,TokenLine+1,""},"-----" ++ [ lists:last(TokenChars) ]}.
-module :
  {token,{prep_module,TokenLine}}.
-----export :
  {token,{prep_export,TokenLine}}.
-----([^e]|e[^x]|ex[^p])[^\n]* :
  [_,_,_,_|A] = TokenChars
  ,{token,{preproc,TokenLine,A}}.
\n\s* :
  [_|A] = TokenChars
  ,{token,{indent,TokenLine+1,A}}. %+1 Since the line is counted before the \n
\s+ :
  skip_token.
-> :
  {token,{'->',TokenLine,TokenChars}}.
== :
  {token,{'==',TokenLine,TokenChars}}.
/= :
  {token,{'/=',TokenLine,TokenChars}}.
=\:= :
  {token,{'=:=',TokenLine,TokenChars}}.
=/= :
  {token,{'=/=',TokenLine,TokenChars}}.
>= :
  {token,{'>=',TokenLine,TokenChars}}.
<= :
  {token,{'<=',TokenLine,TokenChars}}.
\+\+ :
  {token,{'++',TokenLine,TokenChars}}.
\-\- :
  {token,{'--',TokenLine,TokenChars}}.
\$[^\\] :
  {token,{'char_expr',TokenLine,TokenChars}}.
\$\\\\ :
  {token,{'char_expr',TokenLine,TokenChars}}.
\$\\x{\d+} :
  {token,{'char_expr',TokenLine,TokenChars}}.
case :
  {token,{'case',TokenLine}}.
of :
  {token,{'of',TokenLine}}.
if :
  {token,{'if',TokenLine,TokenChars}}.
when :
  {token,{'when',TokenLine,TokenChars}}.
orelse :
  {token,{'orelse',TokenLine,TokenChars}}.
andalso :
  {token,{'andalso',TokenLine,TokenChars}}.
fun :
  {token,{'fun',TokenLine}}.
and :
  {token,{'and',TokenLine,TokenChars}}.
or :
  {token,{'or',TokenLine,TokenChars}}.
xor :
  {token,{'xor',TokenLine,TokenChars}}.
not :
  {token,{'not',TokenLine,TokenChars}}.
end :
  skip_token.
when :
  {token,{'when',TokenLine}}.
receive :
  {token,{'receive',TokenLine}}.
after :
  {token,{'after',TokenLine}}.
try :
  {token,{'try',TokenLine,TokenChars}}.
catch :
  {token,{'catch',TokenLine,TokenChars}}.
\"[^\\"]*(\\.([^\\"]*))*\" :
  %[_|H] = TokenChars
  %,J = element(1, lists:split(length(H) - 1, H))
  %,{token,{string,TokenLine,J}}.
  {token,{string,TokenLine,TokenChars}}.
{D}+ :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.
[a-z][0-9a-zA-Z_]*(\.[0-9a-zA-Z_]+)* :
  {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
\?[A-Z][0-9a-zA-Z_]* :
  {token,{macro,TokenLine,TokenChars}}.
\'[^\\']*(\\.([^\\']*))*\' :
  %[_|H] = TokenChars
  %,J = element(1, lists:split(length(H) - 1, H))
  %,{token,{atom,TokenLine,list_to_atom(J)}}.
  {token,{atom,TokenLine,TokenChars}}.
[A-Z_][0-9a-zA-Z_]* :
  {token,{variable,TokenLine,TokenChars}}.
[\[\]\{\}\\+\-\*\/%\:\;\|\(\)!><=\.\,=\#] :
  [H|_]=TokenChars
  ,{token,{list_to_atom([H]),TokenLine,TokenChars}}.
  
Erlang code.
