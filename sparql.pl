:- use_module(library(readutil)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(date)).
:- use_module(library(pcre)).

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
  convlist(parse_row, RawRows, AllRows).

% Parse a label from Wikidata's SPARQL output. This may be one of the following:
% - a translated text label: literal(lang(en, 'Canada'))
% - a date literal: literal(type('http://www.w3.org/2001/XMLSchema#dateTime', '2021-12-05T00:00:00Z'))
%   - Currently we only support converting these to YEARS
% - a decimal literal: literal(type('http://www.w3.org/2001/XMLSchema#decimal', "88"))
parse_label(literal(lang(_, Label)), Label).
% Date literals
parse_label(literal(type('http://www.w3.org/2001/XMLSchema#dateTime', ISO8601DateTimeLiteral)), Year) :-
  parse_time(ISO8601DateTimeLiteral, iso_8601, UnixTimestamp),
  stamp_date_time(UnixTimestamp, date(Year, _, _, _, _, _, _, _, _), 'UTC').
% Decimal literals - these are normally exported as an atom??
parse_label(literal(type('http://www.w3.org/2001/XMLSchema#decimal', DecimalLiteral)), Output) :-
  atom(DecimalLiteral), atom_string(DecimalLiteral, DecimalLiteralS), number_string(Output, DecimalLiteralS).
parse_label(literal(type('http://www.w3.org/2001/XMLSchema#decimal', DecimalLiteral)), Output) :-
  number_string(Output, DecimalLiteral).
% LITERAL literals. For these we drop default query item formatting (e.g. Q123), but accept others
% as they could be user constructed question text (e.g. "City, Country")
parse_label(literal(Label), Label) :- \+ re_match("^Q[0-9]+$", Label).
% Log failures for easier debugging
parse_label(Item) :- writeln(format("Failed to parse answer label: ~w", Item)), fail.

% Parse a list of labels from Wikidata's SPARQL output
parse_label_list('$null$', []).
parse_label_list(literal(lang(_, RawLabel)), AltLabels) :-
  split_string(RawLabel, ",", " ", AltLabels).
parse_label_list(literal(RawLabel), AltLabels) :-
  string(RawLabel), split_string(RawLabel, ",", " ", AltLabels).
parse_label_list(literal(RawLabel), AltLabels) :-
  atom(RawLabel), atom_string(RawLabel, RawLabelS), split_string(RawLabelS, ",", " ", AltLabels).

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
  parse_label(LHSLabelRaw, LHSLabel), !,
  parse_label(RHSLabelRaw, RHSLabel), !,
  parse_label_list(RHSAltLabelsRaw, RHSAltLabels), !.

% is_valid_row(Row) produces true if a data row can be parsed in the format we expect
% This in particular excludes rows with missing labels for either the question object or default answer,
% as we can't meaningfully generate questions for those!
is_valid_row(Row) :- parse_row(Row, _).
