#!/bin/sh
#Thank you http://code.google.com/p/plists for being a good skeleton package
#for a code library.
erl -eval "leex:file(\"src/neaterl_lex\")" -run init stop -noshell
erl -eval "yecc:file(\"src/neaterl_yec\")" -run init stop -noshell
erl -run make all -run init stop -noshell
erl -pa "ebin/" -eval "neaterl:make_all()" -run init stop -noshell
erl -pa "ebin/" -eval "neaterl:test()" -run init stop -noshell