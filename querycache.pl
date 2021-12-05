% Save query results to disk to avoid excessive queries to Wikidata

:- use_module(library(term_to_json)).
:- use_module(library(http/json)).
:- ensure_loaded("json_deserialize.pl").


cache_dir("cached-results").

% Produces the cache path for a query (a TSV based off the file base name in cached-results/)
% e.g. the following produce true:
%  get_cache_path("queries/test.rq", "cached-results/test.rq.tsv")
%  get_cache_path("/home/james/fun-questions.rq", "cached-results/fun-questions.tsv")
get_cache_path(QueryPath, CachePath) :-
  file_base_name(QueryPath, BaseName),
  cache_dir(CacheDir),
  atomics_to_string([CacheDir, "/", BaseName, ".json"], CachePath).

% Save query results in JSON format
save_query_results(QueryPath, AllRows) :-
  get_cache_path(QueryPath, CachePath),
  term_to_json(AllRows, JSONRows),
  open(CachePath, write, Stream),
  json_write_dict(Stream, JSONRows).


% Load query results from JSON
load_query_results(QueryPath, OutputRows) :-
  get_cache_path(QueryPath, CachePath),
  open(CachePath, read, Stream),
  json_read_dict(Stream, JSONRows),
  json_deserialize(JSONRows, OutputRows).
