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
