:- ensure_loaded("json_deserialize.pl").

:- begin_tests('json_deserialize').

test('json_deserialize - number', [nondet]) :-
    json_deserialize(123, 123).

test('json_deserialize - string', [nondet]) :-
    json_deserialize("foo", "foo").

test('json_deserialize - lists', [nondet]) :-
    json_deserialize(["foo", "bar", "baz"], ["foo", "bar", "baz"]).

test('json_deserialize - term of arity 0', [nondet]) :-
    json_deserialize(foo{functor: "abcdef", args: []}, abcdef).

test('json_deserialize - term of arity 2', [nondet]) :-
    json_deserialize(_{functor: "xyz", args: ["hello", "world"]}, xyz("hello", "world")).

test('json_deserialize - nested terms', [nondet]) :-
    Nested1 = _{functor: "nest", args: ["of", 9000, "birds"]},
    Nested2 = _{functor: "leaf", args: []},
    json_deserialize(_{functor: "tree", args: ["branch", Nested1, Nested2]},
                     tree("branch", nest("of", 9000, "birds"), leaf)).

test('json_deserialize - lists containing terms', [nondet]) :-
    Nested1 = _{functor: "thing", args: []},
    Nested2 = _{functor: "other_thing", args: [Nested1]},
    json_deserialize([Nested1, Nested2], [thing, other_thing(thing)]).

test('json_deserialize - unreasonably complicated', [nondet]) :-
    Nested1 = _{functor: "thing", args: []},
    Nested2 = _{functor: "other_thing", args: [Nested1]},
    json_deserialize([Nested1, Nested2, [Nested2, "foo", [Nested1]]],
                     [thing, other_thing(thing), [other_thing(thing), "foo", [thing]]]).

test('json_deserialize - invalid object (empty)', [fail]) :-
    json_deserialize([1, 2, _{}], _).
test('json_deserialize - invalid object', [fail]) :-
    json_deserialize(_{functor: "abcdef", asfdsafsafas: []}, _).

:- end_tests('json_deserialize').
