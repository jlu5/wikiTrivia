#!/usr/bin/env swipl

:- initialization(main, main).
:- ensure_loaded("cliopts.pl").

% Just for testing the CLI parser!
main(Argv) :- parse_cli_args(Argv, Opts), writeln(Opts).
