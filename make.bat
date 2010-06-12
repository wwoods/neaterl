mkdir ebin
erl -eval "leex:file(\"src/neaterl_lex\")" -run init stop -noshell
erl -eval "yecc:file(\"src/neaterl_yec\")" -run init stop -noshell
erl -run make all -run init stop -noshell
erl -pa "ebin/" -eval "neaterl:make_all()" -run init stop -noshell
erl -pa "ebin/" -eval "neaterl:test()" -run init stop -noshell
