#!/bin/sh
#Thank you http://code.google.com/p/plists for being a good skeleton package
#for a code library.
erl -eval "leex:file(\"src/prettyerl_lex\")" -run init stop -noshell
erl -eval "yecc:file(\"src/prettyerl_yec\")" -run init stop -noshell
erl -run make all -run init stop -noshell