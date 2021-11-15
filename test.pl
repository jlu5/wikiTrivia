% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

:- ensure_loaded("sparql.pl").
:- ensure_loaded("main.pl").

:- begin_tests('parse_question_label').

test('parse_question_label success - translated literal', [nondet]) :-
  parse_question_label(literal(lang(en, "Hello world")), "Hello world").
test('parse_question_label fail - bare literal', [fail]) :-
  parse_question_label(literal(1234), _).
test('parse_question_label fail - mismatch', [fail]) :-
  parse_question_label(literal(lang(en, "Hello world")), "dlrow olleH").

:- end_tests('parse_question_label').

:- begin_tests('parse_answer_label').

test('parse_answer_label success - translated literal', [nondet]) :-
  parse_answer_label(literal(lang(en, "Hello world")), "Hello world").
test('parse_answer_label success - bare literal', [nondet]) :-
  parse_answer_label(literal('2000-01-01T00:00:00Z'), '2000-01-01T00:00:00Z').
test('parse_answer_label fail - output mismatch', [fail]) :-
  parse_answer_label(literal(lang(en, "Hello world")), "dlrow olleH").
test('parse_answer_label fail - invalid input', [fail]) :-
  parse_answer_label(notAliteral(lang(abcdef, "312312312")), _).

:- end_tests('parse_answer_label').

:- begin_tests('parse_label_list').

test('parse_label_list success - single item', [nondet]) :-
  parse_label_list(literal(lang(en, "Hello world")), ["Hello world"]).
test('parse_label_list success - multiple items', [nondet]) :-
  parse_label_list(literal(lang(en, "A, B, C d e, f")), ["A", "B", "C d e", "f"]).
test('parse_label_list success - null', [nondet]) :-
  parse_label_list('$null$', []).
test('parse_label_list fail - bare literal', [fail]) :-
  parse_label_list(literal('2000-01-01T00:00:00Z'), _).
test('parse_label_list fail - mismatched output', [fail]) :-
  parse_label_list(literal(lang(en, "A, B, C d e, f g")), []).

:- end_tests('parse_label_list').

:- begin_tests('parse_row').

test('parse_row success - with alt labels', [nondet]) :-
  parse_row(row('http://www.wikidata.org/entity/Q1930',
                literal(lang(en, 'Ottawa')),
                'http://www.wikidata.org/entity/Q16',
                literal(lang(en, 'Canada')),
                literal(lang(en, 'CA, ca, can, CAN, British North America, Can., Dominion of Canada'))),
            % LHSNode
            'http://www.wikidata.org/entity/Q1930',
            % LHSLabel
            'Ottawa',
            % RHSNode
            'http://www.wikidata.org/entity/Q16',
            % RHSLabel
            'Canada',
            % RHSAltLabels
            ["CA", "ca", "can", "CAN", "British North America", "Can.", "Dominion of Canada"]
            ).

test('parse_row success - date as answer', [nondet]) :-
  parse_row(row('http://www.wikidata.org/entity/Q2665141',
                literal(lang(en, 'SWI-Prolog')),
                literal(type('http://www.w3.org/2001/XMLSchema#dateTime', '1987-01-01T00:00:00Z')),
                literal('1987-01-01T00:00:00Z'),
                '$null$'),
            'http://www.wikidata.org/entity/Q2665141',
            'SWI-Prolog',
            literal(type('http://www.w3.org/2001/XMLSchema#dateTime', '1987-01-01T00:00:00Z')),
            % When we implement numerical scoring, we should parse this more completely
            '1987-01-01T00:00:00Z',
            []
            ).

test('parse_row fail - mismatched output', [fail]) :-
  parse_row(row('http://www.wikidata.org/entity/Q1930',
                literal(lang(en, 'Ottawa')),
                'http://www.wikidata.org/entity/Q16',
                literal(lang(en, 'Canada')),
                literal(lang(en, 'CA, ca, can, CAN, British North America, Can., Dominion of Canada'))),
            'http://www.wikidata.org/entity/Q1930',
            'Ottawa',
            'http://www.wikidata.org/entity/Q16',
            'Canada',
            []
            ).

test('parse_row fail - missing LHS label', [fail]) :-
  parse_row(row('http://www.wikidata.org/entity/Q4418164',
                literal('Q4418164'), % missing LHS label
                'http://www.wikidata.org/entity/Q159',
                literal(lang(en, 'Russia')),
                literal(lang(en, 'RF, RUS, Rus, RU, Russian Federation, ru, Rossija, Rossiya, Rossijskaja Federatsija, Rossiyskaya Federatsiya'))),
            _LHSNode, _LHSLabel, _RHSNode, _RHSLabel, _RHSAltLabels).

:- end_tests('parse_row').

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