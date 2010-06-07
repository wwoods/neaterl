
Definitions.

D = [0-9]

Rules.

\\\n :
  skip_token.
\n\s* :
  [_|A] = TokenChars
  ,{token,{indent,TokenLine+1,A}}. %+1 Since the line is counted before the \n
\s+ :
  skip_token.
\t+ :
  skip_token.
\%[^\n]* :
  skip_token.
-> :
  {token,{'->',TokenLine,TokenChars}}.
== :
  {token,{'==',TokenLine,TokenChars}}.
>= :
  {token,{'>=',TokenLine,TokenChars}}.
<= :
  {token,{'<=',TokenLine,TokenChars}}.
\+\+ :
  {token,{'++',TokenLine,TokenChars}}.
\-\- :
  {token,{'--',TokenLine,TokenChars}}.
\"[^\\"]*(\\.([^\\"]*))*\" :
  %[_|H] = TokenChars
  %,J = element(1, lists:split(length(H) - 1, H))
  %,{token,{string,TokenLine,J}}.
  {token,{string,TokenLine,TokenChars}}.
{D}+ :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.
[a-z][0-9a-zA-Z_]* :
  {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
\'[^\\']*(\\.([^\\']*))*\' :
  %[_|H] = TokenChars
  %,J = element(1, lists:split(length(H) - 1, H))
  %,{token,{atom,TokenLine,list_to_atom(J)}}.
  {token,{atom,TokenLine,TokenChars}}.
[A-Z][0-9a-zA-Z_]* :
  {token,{variable,TokenLine,TokenChars}}.
[\[\]\{\}\\+\-\*\/%\:\;\|\(\)!><=\.\,=] :
  [H|_]=TokenChars
  ,{token,{list_to_atom([H]),TokenLine,TokenChars}}.
-module :
  {token,{prep_module,TokenLine}}.
-export :
  {token,{prep_export,TokenLine}}.
case :
  {token,{'case',TokenLine}}.
if :
  {token,{'if',TokenLine}}.
end :
  {token,{'end',TokenLine}}.
when :
  {token,{'when',TokenLine}}.
orelse :
  {token,{'orelse',TokenLine}}.
andalso :
  {token,{'andalso',TokenLine}}.
fun :
  {token,{'fun',TokenLine}}.
not :
  {token,{'not',TokenLine}}.
end :
  skip_token.
when :
  {token,{'when',TokenLine}}.
  
Erlang code.
