% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

% More tests in other files!!
:- ensure_loaded("test-json_deserialize.pl").
:- ensure_loaded("test-querycache.pl").
:- ensure_loaded("test-sparql.pl").

:- ensure_loaded("cli.pl").

:- begin_tests('print_known_topics').

% test needs to be updated when we add more topics
test('print_known_topics success - test if prints out expected output') :-
  known_topics(AllTopics),
  with_output_to(atom(Output), print_known_topics(AllTopics, 1)),
  re_match("^1\\. Capital cities.*\\n2\\. 1000 Vital Wikipedia.*\\n7\\..*"/ms, Output).

:- end_tests('print_known_topics').

:- begin_tests('ask_and_score_questions').

% tests need to be updated when we choose variable number of questions
test('ask_and_score_questions successful output with final score 0', Output == 'Your final score is: 0.00/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 0, 5, _)).

test('ask_and_score_questions successful output with final score 3', Output == 'Your final score is: 3.00/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 3, 5, _)).

test('ask_and_score_questions successful output with decimal value possible score', Output == 'Your final score is: 3.75/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 3.75, 5, _)).

test('ask_and_score_questions successful output with max possible score', Output == 'Your final score is: 5.00/5!\n') :-
  with_output_to(atom(Output), ask_and_score_questions(_, _, 0, 5, 5, _)).

test('score_answer fail non-numerical canonical answer', [fail]) :-
  with_output_to(atom(_), score_answer("Afreeca", "Africa", ["Africa"], _, 2, 1)).

test('score_answer fail non-numerical alt answer', [fail]) :-
  with_output_to(atom(_), score_answer("af", "Africa", ["afd", "nasd"], _, 2, 1)).

test('score_answer exact numerical canonical answer', Output == 'Close! The correct answer was 2000 \n') :-
  with_output_to(atom(Output), score_answer('2000', 2000, [2000], 20, _, 1)).

test('score_answer not close numerical canonical answer', Output == 'You were off by over 20! The answer was 2000 \n') :-
  with_output_to(atom(Output), score_answer('0', 2000, [2000], 20, _, 0)).

test('score_answer close numerical canonical answer', Output == 'Close! The correct answer was 2000 \n') :-
  with_output_to(atom(Output), score_answer('1990', 2000, [2000], 20, _, 0.5)).


:- end_tests('ask_and_score_questions').

:- begin_tests('give_hint'). 

test('give empty lines hint single letter', Output == 'HINT: _ \n') :-
  with_output_to(atom(Output), give_hint("H", 2)).

test('give first letter hint single letter', Output == 'HINT: H \n') :-
  with_output_to(atom(Output), give_hint("H", 1)).

test('give empty lines hint', Output == 'HINT: _ _ _ _ _ \n') :-
  with_output_to(atom(Output), give_hint("Hello", 2)).

test('give first letter hint', Output == 'HINT: H _ _ _ _ \n') :-
  with_output_to(atom(Output), give_hint("Hello", 1)).

:- end_tests('give_hint').

:- begin_tests('member_case_insensitive'). 

test('member_case_insensitive member in list', [nondet]) :-
  member_case_insensitive("Hello", ["Hello", "asldkaksd"]).

test('member_case_insensitive member not in list', [fail]) :-
  member_case_insensitive("Hell", ["Hello", "asldkaksd"]).

test('member_case_insensitive member in list lower case', [nondet]) :-
  member_case_insensitive("hello", ["Hello", "asldkaksd", "tmkrmijirfgj"]).

test('member_case_insensitive member not in list lower case', [fail]) :-
  member_case_insensitive("hsad", ["Hello", "asldkaksd", "tmkrmijirfgj"]).

:- end_tests('member_case_insensitive'). 