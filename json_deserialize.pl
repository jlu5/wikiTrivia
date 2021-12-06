% SWI-Prolog includes a term_to_json/2 but no inverse... so I went ahead and wrote one?!?!
% I figured this was still the easiest way to save and load arbitrary structures without
% having to pre-declare everything -JL
% Ref: https://swi-prolog.discourse.group/t/built-in-options-for-converting-arbitrary-prolog-term-to-json/3783/6

% Recurse over lists
json_deserialize(List, Output) :- is_list(List), maplist(json_deserialize, List, Output).
% Deserialize complex terms, which are exported as a JSON object
json_deserialize(_{functor: Functor, args: Args}, Output) :-
  atom_chars(FunctorAtom, Functor),
  json_deserialize(Args, DeserializedArgs),
  append([FunctorAtom], DeserializedArgs, ExpandedArgs),
  Output =.. ExpandedArgs.
% leave everything that isn't a dict or list as is
json_deserialize(Term, Term) :- \+ is_dict(Term), \+ is_list(Term).
