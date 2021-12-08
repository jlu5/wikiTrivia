max_chances(3).

% build_hint/3
build_hint(_, 0, 0) :-
  writeln("").

% build_hint/3
% handles any space cases
build_hint([' '| Rest], Length, 0) :-
  write("  "),
  LettersLeft is Length - 1,
  build_hint(Rest, LettersLeft, 0).

% build_hint/3
% This prints out the letters that are hidden as "_". This is called once NumTries reaches 0.
build_hint([_ | Rest], Length, 0) :-
  write("_ "),
  LettersLeft is Length - 1,
  build_hint(Rest, LettersLeft, 0).

% build_hint/3
% This prints out the letters that should be given as hints. This is called whenever NumTries is > 0.
build_hint([Head | Rest], Length, NumTries) :-
  write(Head),
  write(" "),
  LettersLeft is Length - 1,
  TriesLeft is NumTries - 1,
  build_hint(Rest, LettersLeft, TriesLeft).

% give_hint/2
% when called gives hint and breaks up CanonicalAnswer into a char list
% NumTries Indicates the number of attempts the user has done as a guess, and is used to see how much of the letter should be revealed.
give_hint(CanonicalAnswer, NumAttemptsRemaining) :-
  max_chances(MaxChances),
  NumTries is MaxChances - NumAttemptsRemaining,
  string_length(CanonicalAnswer, Length),
  string_chars(CanonicalAnswer, Chars),
  write("HINT: "),
  build_hint(Chars, Length, NumTries).
