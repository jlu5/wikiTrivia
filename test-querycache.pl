:- ensure_loaded("querycache.pl").
:- begin_tests('get_cache_path').

test('get_cache_path success - relative RQ path', [nondet]) :-
    get_cache_path("queries/test.rq", "cached-results/test.rq.json").

test('get_cache_path success - absolute RQ path', [nondet]) :-
    get_cache_path("/home/james/fun-questions.rq", "cached-results/fun-questions.rq.json").

test('get_cache_path success - relative RQ path 2', [nondet]) :-
    get_cache_path("wikitrivia.rq", "cached-results/wikitrivia.rq.json").

test('get_cache_path fail - mismatch', [fail]) :-
    get_cache_path("/home/james/fun-questions.rq", "foo").

:- end_tests('get_cache_path').

:- begin_tests('cache round trip').

% FIXME: these should really be using a temporary folder so they don't pollute the local cache
test('query cache round trip - all string literals', [nondet]) :-
    Data = [
        ["http://www.wikidata.org/entity/Q1","totality consisting of space, time, matter and energy","http://www.wikidata.org/entity/Q1","Universe",["Heaven and earth","metagalaxy","The Universe","system","all","outer space","space","world","creation","cosmos","The Cosmos","everything","existence","macrocosm","Yin and Yang"]],
        ["http://www.wikidata.org/entity/Q2","third planet from the Sun in the Solar System","http://www.wikidata.org/entity/Q2","Earth",["Planet Earth","globe","Blue Planet","world","the world","the Earth"]],
        ["http://www.wikidata.org/entity/Q3","matter capable of extracting energy from the environment for replication","http://www.wikidata.org/entity/Q3","life",["biota"]],
        ["http://www.wikidata.org/entity/Q4","permanent cessation of vital functions","http://www.wikidata.org/entity/Q4","death",["dead","deaths","oblivion","dies","cessation","morbidity","bereft of life","deceased","fallen","fatal","final rest","has died","has kicked the bucket","has passed away","has succumbed","meeting the Reaper"]]
    ],
    save_query_results("cachetest-1.rq", Data),
    load_query_results("cachetest-1.rq", ReloadedData),
    ReloadedData = Data.

test('query cache round trip - complex literals', [nondet]) :-
    Data = [
        ["http://www.wikidata.org/entity/Q192890","Vancouver Canucks",literal(type("http://www.w3.org/2001/XMLSchema#dateTime","1970-01-01T00:00:00Z")),"1970-01-01T00:00:00Z",[]],
        ["http://www.wikidata.org/entity/Q196107","Vancouver Whitecaps FC",literal(type("http://www.w3.org/2001/XMLSchema#dateTime","2009-01-01T00:00:00Z")),"2009-01-01T00:00:00Z",[]],
        ["http://www.wikidata.org/entity/Q370248","HSBC Bank Canada",literal(type("http://www.w3.org/2001/XMLSchema#dateTime","1981-01-01T00:00:00Z")),"1981-01-01T00:00:00Z",[]],
        ["http://www.wikidata.org/entity/Q391028","University of British Columbia",literal(type("http://www.w3.org/2001/XMLSchema#dateTime","1908-01-01T00:00:00Z")),"1908-01-01T00:00:00Z",[]],
        ["http://www.wikidata.org/entity/Q610918","Mountain Equipment Co-op",literal(type("http://www.w3.org/2001/XMLSchema#dateTime","1971-01-01T00:00:00Z")),"1971-01-01T00:00:00Z",[]]
    ],
    save_query_results("cachetest-2.rq", Data),
    load_query_results("cachetest-2.rq", ReloadedData),
    ReloadedData = Data.

:- end_tests('cache round trip').
