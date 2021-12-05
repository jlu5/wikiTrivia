% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

% More tests in other files!!
:- ensure_loaded("test-sparql.pl").

:- ensure_loaded("main.pl").

:- begin_tests('print_known_topics').

% test needs to be updated when we add more topics
test('print_known_topics success - test if prints out expected output', Output == '1. Capital cities of various countries\n2. 1000 Vital Wikipedia topics (general Q&A)\n3. Billion euro companies... and where they\'re from\n') :-
  known_topics(AllTopics),
  with_output_to(atom(Output), print_known_topics(AllTopics, 1)).

:- end_tests('print_known_topics').

:- begin_tests('ask_and_score_questions').

% tests need to be updated when we choose variable number of questions
test('ask_and_score_questions successful output with final score 0', Output == 'Your final score is: 0/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 0)).

test('ask_and_score_questions successful output with final score 3', Output == 'Your final score is: 3/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 3)).

test('ask_and_score_questions successful output with max possible score', Output == 'Your final score is: 5/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 5)).

:- end_tests('ask_and_score_questions').
