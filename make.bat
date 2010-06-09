
erl -eval "leex:file(\"src/prettyerl_lex\")" -run init stop -noshell
erl -eval "yecc:file(\"src/prettyerl_yec\")" -run init stop -noshell
erl -run make all -run init stop -noshell
erl -pa "ebin/" -eval "prettyerl:test()" -run init stop -noshell