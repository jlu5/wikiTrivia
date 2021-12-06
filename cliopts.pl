:- use_module(library(optparse)).
:- use_module(library(option)).

optsspec(
    [ [opt(rounds), type(integer), default(5),
        shortflags([n]),longflags(["num-rounds"]),
        help('number of rounds to play')]

    , [opt(scoringrange), type(integer), default(20),
        shortflags(['R']),longflags(["scoring-range"]),
        help('range size of accepted answers for numerical based scoring')]

    , [opt(queryfile), type(atom), meta('FILE'),
        shortflags([f]), longflags(["query-file"]),
        help('load a SPARQL query from FILE instead of prompting')]

    , [opt(questionformat), type(atom), meta('QFORMAT'), default('Question: ~w'),
        shortflags([q]), longflags(["question-format"]),
        help('string to format questions; this should include a ~w to substitute the question item')]

    , [opt(help), type(boolean), default(false),
        shortflags([h]),longflags(["help"]),
        help('show this help text')]
    ]).

check_show_help(true) :-
  optsspec(OptsSpec),
  opt_help(OptsSpec, Help),
  writeln(Help),
  halt.
check_show_help(false).

parse_cli_args(Argv, cliopts(QueryFile, QuestionFormatString, ScoringRange, NumRounds)) :-
  optsspec(OptsSpec),
  writeln("wikiTrivia - a trivia quiz game based off Wikidata relations"),
  opt_parse(OptsSpec, Argv, Opts, _PositionalArgs),

  option(rounds(NumRounds), Opts),
  option(scoringrange(ScoringRange), Opts),
  option(queryfile(QueryFile), Opts), % allowed to be free I guess
  option(questionformat(QuestionFormatString), Opts),
  option(help(ShowHelp), Opts),
  check_show_help(ShowHelp).
