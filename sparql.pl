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
