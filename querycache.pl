% Save query results to disk to avoid excessive queries to Wikidata

:- use_module(library(term_to_json)).
:- use_module(library(http/json)).
:- ensure_loaded("json_deserialize.pl").

cache_dir("cached-results").

% Produces the cache path for a query (.json based off the file base name, in cached-results/)
% e.g. the following produce true:
%  get_cache_path("queries/test.rq", "cached-results/test.rq.json")
%  get_cache_path("/home/james/fun-questions.rq", "cached-results/fun-questions.json")
get_cache_path(QueryPath, CachePath) :-
  file_base_name(QueryPath, BaseName),
  cache_dir(CacheDir),
  atomics_to_string([CacheDir, "/", BaseName, ".json"], CachePath).

% Save query results in JSON format
save_query_results(QueryPath, AllRows) :-
  get_cache_path(QueryPath, CachePath),
  access_file(CachePath, write),
  term_to_json(AllRows, JSONRows),
  open(CachePath, write, Stream),
  json_write_dict(Stream, JSONRows),
  close(Stream).

% Load query results from JSON
load_query_results(QueryPath, OutputRows) :-
  get_cache_path(QueryPath, CachePath),
  access_file(CachePath, read),  % file exists & is accessible
  open(CachePath, read, Stream),
  json_read_dict(Stream, JSONRows),
  json_deserialize(JSONRows, OutputRows),
  close(Stream).
