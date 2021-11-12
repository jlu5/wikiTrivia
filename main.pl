:- use_module(library(readutil)).
:- ensure_loaded("sparql.pl").

known_topics([
  quiz_topic("queries/capitals-to-countries.rq", "Capital cities of various countries"),
  quiz_topic("queries/1000-vital-wikipedia-topics.rq", "1000 Vital Wikipedia topics (general Q&A)"),
  quiz_topic("queries/billion-euro-companies.rq", "Billion euro companies... and where they're from")
  % Enable these once we have a more lenient way of scoring numerical answers!
  %quiz_topic("queries/inception-vancouver-companies.rq", "Vancouver-based companies and when they were founded"),
  %quiz_topic("queries/inception-oses-programming-langs.rq", "Birth dates of programming langs and operating systems")
]).

print_known_topics([], _).
print_known_topics([quiz_topic(_Filename, TopicName)|Rest], CurrentIndex) :-
  write(CurrentIndex), write(". "), writeln(TopicName), NewIndex is CurrentIndex + 1, print_known_topics(Rest, NewIndex).

choose_topic(QuizTopic) :-
  known_topics(AllTopics),
  print_known_topics(AllTopics, 1),
  read_line_to_string(user_input, InputString),
  number_string(Index, InputString),
  nth1(Index, AllTopics, QuizTopic).
choose_topic(QuizTopic) :- writeln("Invalid input"), choose_topic(QuizTopic).

main(_) :-
  choose_topic(quiz_topic(TopicFilename, TopicDescription)), !.
  %get_all_from_rq_file(TopicFilename, AllRows).
