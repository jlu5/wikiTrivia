:- ensure_loaded("sparql.pl").

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
            [
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
            ]).

test('parse_row success - date as answer', [nondet]) :-
  parse_row(row('http://www.wikidata.org/entity/Q2665141',
                literal(lang(en, 'SWI-Prolog')),
                literal(type('http://www.w3.org/2001/XMLSchema#dateTime', '1987-01-01T00:00:00Z')),
                literal('1987-01-01T00:00:00Z'),
                '$null$'),
            [
                'http://www.wikidata.org/entity/Q2665141',
                'SWI-Prolog',
                literal(type('http://www.w3.org/2001/XMLSchema#dateTime', '1987-01-01T00:00:00Z')),
                % When we implement numerical scoring, we should parse this more completely
                '1987-01-01T00:00:00Z',
                []
            ]).

test('parse_row fail - mismatched output', [fail]) :-
  parse_row(row('http://www.wikidata.org/entity/Q1930',
                literal(lang(en, 'Ottawa')),
                'http://www.wikidata.org/entity/Q16',
                literal(lang(en, 'Canada')),
                literal(lang(en, 'CA, ca, can, CAN, British North America, Can., Dominion of Canada'))),
            [
                'http://www.wikidata.org/entity/Q1930',
                'Ottawa',
                'http://www.wikidata.org/entity/Q16',
                'Canada',
                []
            ]
            ).

test('parse_row fail - missing LHS label', [fail]) :-
  parse_row(row('http://www.wikidata.org/entity/Q4418164',
                literal('Q4418164'), % missing LHS label
                'http://www.wikidata.org/entity/Q159',
                literal(lang(en, 'Russia')),
                literal(lang(en, 'RF, RUS, Rus, RU, Russian Federation, ru, Rossija, Rossiya, Rossijskaja Federatsija, Rossiyskaya Federatsiya'))),
            _Result).

:- end_tests('parse_row').
