:- use_module(library(readutil)).
:- use_module(library(semweb/sparql_client)).

% Returns rows for a SPARQL query given a filename to load the query from
% e.g. get_rows_from_query_file("queries/capitals-to-countries.rq", Row)
get_rows_from_rq_file(Filename, Row) :-
  read_file_to_string(Filename, RequestString, []),
  sparql_query(RequestString,
               Row,
               [ scheme(https),
                 host('query.wikidata.org'),
                 path('/sparql')
               ]).

% Returns all rows for a SPARQL query given a filename to load the query from
get_all_from_rq_file(Filename, AllRows) :-
  findall(Row, get_rows_from_rq_file(Filename, Row), RawRows),
  % Exclude entries that are missing a label on either LHS or RHS
  include(is_valid_row, RawRows, AllRows).

% Parse a Q&A Row into its constituent components, where rows are in the form:
% Row = row('http://www.wikidata.org/entity/Q1930',
%           literal(lang(en, 'Ottawa')),
%           'http://www.wikidata.org/entity/Q16',
%           literal(lang(en, 'Canada')),
%           literal(lang(en, 'CA, ca, can, CAN, British North America, Can., Dominion of Canada')))
% Each row item corresponds to a variable (?abcd) selected in the SPARQL query, with the expected
% order as shown.
parse_row(Row, LHSNode, LHSLabel, RHSNode, RHSLabel, RHSAltLabels) :-
  Row = row(LHSNode, literal(lang(_, LHSLabel)), RHSNode, literal(lang(_, RHSLabel)),
            % Alt labels are split by ","
            literal(lang(_, RHSAltLabelsRaw))), split_string(RHSAltLabelsRaw, ",", " ", RHSAltLabels).

is_valid_row(Row) :- parse_row(Row, _, _, _, _, _).
