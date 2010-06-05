
Definitions.

D = [0-9]

Rules.

\s+ :
  skip_token.
\t+ :
  skip_token.
\%[^\n]* :
  skip_token.
\\\n :
  skip_token.
[,\n] :
  {token,{separator,TokenLine}}.
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
\. :
  {token,{'.',TokenLine,TokenChars}}.
\"[^\\"]*(\\.([^\\"]*))*\" :
  [_|H] = TokenChars
  ,J = element(1, lists:split(length(H) - 1, H))
  ,{token,{string,TokenLine,J}}.
{D}+ :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.
[a-z][0-9a-zA-Z_]* :
  {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
\'[^\\']*(\\.([^\\']*))*\' :
  [_|H] = TokenChars
  ,J = element(1, lists:split(length(H) - 1, H))
  ,{token,{atom,TokenLine,list_to_atom(J)}}.
[A-Z][0-9a-zA-Z_]* :
  {token,{variable,TokenLine,TokenChars}}.
[\[\]\{\}\\+\-\*\/%\:\|\(\)] :
  [H|_]=TokenChars
  ,{token,{list_to_atom([H]),TokenLine}}.
  
Erlang code.
