:- use_module(library(readutil)).
:- ensure_loaded("sparql.pl").

% FIXME: make this configurable
num_questions(5).

known_topics([
  quiz_topic("queries/capitals-to-countries.rq", "Capital cities of various countries", "What country is ~w the capital of?"),
  quiz_topic("queries/1000-vital-wikipedia-topics.rq", "1000 Vital Wikipedia topics (general Q&A)", "~w?"),
  quiz_topic("queries/billion-euro-companies.rq", "Billion euro companies... and where they're from", "What country is ~w based in?")
  % Enable these once we have a more lenient way of scoring numerical answers!
  %quiz_topic("queries/inception-vancouver-companies.rq", "Vancouver-based companies and when they were founded"),
  %quiz_topic("queries/inception-oses-programming-langs.rq", "Birth dates of programming langs and operating systems")
]).

print_known_topics([], _).
print_known_topics([quiz_topic(_Filename, TopicName, _FormatString)|Rest], CurrentIndex) :-
  write(CurrentIndex), write(". "), writeln(TopicName),
  NewIndex is CurrentIndex + 1, print_known_topics(Rest, NewIndex).

choose_topic(QuizTopic) :-
  known_topics(AllTopics),
  print_known_topics(AllTopics, 1),
  read_line_to_string(user_input, InputString),
  number_string(Index, InputString),
  nth1(Index, AllTopics, QuizTopic).
choose_topic(QuizTopic) :-
  writeln("Invalid input"),
  choose_topic(QuizTopic).

score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, 1) :-
  AcceptableAnswers = [CanonicalAnswer|AlternativeAnswers],
  % First convert input and accepted answers to lowercase so that matches are case insensitive
  string_lower(UserAnswer, NormalizedUserAnswer),
  maplist(string_lower, AcceptableAnswers, NormalizedAcceptableAnswers),
  member(NormalizedUserAnswer, NormalizedAcceptableAnswers),
  format("Correct! The (canonical) answer is ~w \n", [CanonicalAnswer]).
score_answer(_, CanonicalAnswer, _, 0) :-
  format("Incorrect! The answer is ~w \n", [CanonicalAnswer]).

% ask_questions will take in a Row, prompt the user with the question, and verify the response. It will recurse with further questions until RemainingQuestions becomes 0
ask_questions(_, _, 0, Score) :-
  format("Your final score is: ~d!\n", [Score]).
ask_questions(AllRows, FormatString, RemainingQuestions, CurrentScore) :-
  random_member(Row, AllRows),
  parse_row(Row, _, LHSLabel, _, RHSLabel, RHSAltLabels),
  format(atom(Question), FormatString, [LHSLabel]),
  writeln(""),
  writeln(Question),
  read_line_to_string(user_input, UserAnswer),
  % Score is 0 or 1
  score_answer(UserAnswer, RHSLabel, RHSAltLabels, Score),
  NewScore is CurrentScore + Score,
  format("Your current score is: ~d \n", [NewScore]),
  NewRemainingQuestions is RemainingQuestions - 1,
  ask_questions(AllRows, FormatString, NewRemainingQuestions, NewScore).

play_topic(quiz_topic(TopicFilename, TopicDescription, FormatString)) :-
  writeln("Loading questions from Wikidata..."),
  get_all_from_rq_file(TopicFilename, AllRows),
  format("Playing topic: ~w", [TopicDescription]),

  num_questions(RemainingQuestions),
  ask_questions(AllRows, FormatString, RemainingQuestions, 0).

main(_) :-
  choose_topic(QuizTopic), !,
  play_topic(QuizTopic), !.
