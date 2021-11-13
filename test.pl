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
