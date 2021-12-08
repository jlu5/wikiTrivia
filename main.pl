#!/usr/bin/env swipl

% :- initialization(main, main).

:- use_module(library(readutil)).
:- ensure_loaded("cli.pl").
:- ensure_loaded("cliopts.pl").

% Entrypoint: main/1 prompts the user to choose a quiz topic and then starts the game
main(Argv) :-
  parse_cli_args(Argv, cliopts(QueryFile, QuestionFormatString, ScoringRange, NumRounds)), !,
  parse_or_prompt_topic(QueryFile, QuestionFormatString, QuizTopic), !,
  play_topic(QuizTopic, NumRounds, ScoringRange), !.
