:- use_module(library(readutil)).
:- use_module(library(semweb/sparql_client)).

% Returns raw rows for a SPARQL query given a filename to load the query from
% e.g. get_rows_from_query_file("queries/capitals-to-countries.rq", Row)
get_rows_from_rq_file(Filename, Row) :-
  read_file_to_string(Filename, RequestString, []),
  sparql_query(RequestString,
               Row,
               [ scheme(https),
                 host('query.wikidata.org'),
                 path('/sparql')
               ]).

% Parse and return all rows for a SPARQL query given a filename to load the query from
% This will filter out entries that fail to parse: e.g. those missing a label on either LHS or RHS
get_all_from_rq_file(Filename, AllRows) :-
  findall(Row, get_rows_from_rq_file(Filename, Row), RawRows),
  maplist(parse_row, RawRows, AllRows).

% Parse a label from Wikidata's SPARQL output
% This requires that labels we use to ask questions have have an translatable name attached, while answers may be either a translated names or a plain literal (e.g. a date)
% Wikidata will normally fall back to producing the node ID, which is usually not helpful when asking questions
parse_question_label(literal(lang(_, Label)), Label).
parse_answer_label(literal(lang(_, Label)), Label).
parse_answer_label(literal(Label), Label).

% Parse a list of labels from Wikidata's SPARQL output
parse_label_list('$null$', []).
parse_label_list(literal(lang(_, RawLabel)), AltLabels) :-
  split_string(RawLabel, ",", " ", AltLabels).

% Parse a Q&A Row into its constituent components, where rows are in the form:
% Row = row('http://www.wikidata.org/entity/Q1930',
%           literal(lang(en, 'Ottawa')),
%           'http://www.wikidata.org/entity/Q16',
%           literal(lang(en, 'Canada')),
%           literal(lang(en, 'CA, ca, can, CAN, British North America, Can., Dominion of Canada')))
% Each row item corresponds to a variable (?abcd) selected in the SPARQL query, with the expected
% order as shown.
parse_row(Row, [LHSNode, LHSLabel, RHSNode, RHSLabel, RHSAltLabels]) :-
  Row = row(LHSNode, LHSLabelRaw, RHSNode, RHSLabelRaw, RHSAltLabelsRaw),
  parse_question_label(LHSLabelRaw, LHSLabel), !,
  parse_answer_label(RHSLabelRaw, RHSLabel), !,
  parse_label_list(RHSAltLabelsRaw, RHSAltLabels), !.

% is_valid_row(Row) produces true if a data row can be parsed in the format we expect
% This in particular excludes rows with missing labels for either the question object or default answer,
% as we can't meaningfully generate questions for those!
is_valid_row(Row) :- parse_row(Row, _).
