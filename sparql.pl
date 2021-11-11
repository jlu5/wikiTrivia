:- use_module(library(readutil)).
:- use_module(library(semweb/sparql_client)).

get_data(Row) :-
  read_file_to_string("queries/capitals-to-countries.rq", RequestString, []),
  sparql_query(RequestString,
               Row,
               [ scheme(https),
                 host('query.wikidata.org'),
                 path('/sparql')
               ]).

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
