
:- ensure_loaded("cliopts.pl").
:- ensure_loaded("sparql.pl").
:- ensure_loaded("querycache.pl").

% known_topics stores a list of quiz_topics(Filename, QuizTopicName, QuestionFormatString), where:
%   Filename is the SPARQL query to load to play this topic
%   QuizTopicName is the description fo the topic shown when choosing topics
%   QuestionFormatString is the format/3 string used to generate question text given a Wikidata row
known_topics([
  quiz_topic("queries/capitals-to-countries.rq", "Capital cities of various countries", "What country is ~w the capital of?"),
  quiz_topic("queries/1000-vital-wikipedia-topics.rq", "1000 Vital Wikipedia topics (general Q&A)", "~w?"),
  quiz_topic("queries/billion-euro-companies.rq", "Billion euro companies... and where they're from", "What country is ~w based in?"),
  % Enable these once we have a more lenient way of scoring numerical answers!
  quiz_topic("queries/inception-vancouver-orgs.rq", "Vancouver-based companies and when they were founded", "When was ~w founded?"),
  quiz_topic("queries/inception-oses-programming-langs.rq", "Birth dates of programming langs and operating systems", "When was ~w created?")
]).

%%%

% print_known_topics(QuizTopics, CurrentIndex) prints each quiz topic's name, starting at (one based) CurrentIndex
% and continuing until the list of quiz topics is empty
print_known_topics([], _).
print_known_topics([quiz_topic(_Filename, TopicName, _FormatString)|Rest], CurrentIndex) :-
  write(CurrentIndex), write(". "), writeln(TopicName),
  NewIndex is CurrentIndex + 1, print_known_topics(Rest, NewIndex).

% choose_topic(QuizTopic) prompts for user input and produces the quiz_topic instance chosen by the user
choose_topic(QuizTopic) :-
  known_topics(AllTopics),
  print_known_topics(AllTopics, 1),
  read_line_to_string(user_input, InputString),
  number_string(Index, InputString),
  nth1(Index, AllTopics, QuizTopic).
choose_topic(QuizTopic) :-
  writeln("Invalid input"),
  choose_topic(QuizTopic).


% score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, Score) takes in a user's answer to a question and compares
% it case-insensitively to a list of canonical and alternative (accepted) answers, producing a positive Score
% if the answer is correct and 0 otherwise
score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, 1, _) :-
  AcceptableAnswers = [CanonicalAnswer|AlternativeAnswers],
  % First convert input and accepted answers to lowercase so that matches are case insensitive
  string_lower(UserAnswer, NormalizedUserAnswer),
  maplist(string_lower, AcceptableAnswers, NormalizedAcceptableAnswers),

  member(NormalizedUserAnswer, NormalizedAcceptableAnswers),

  format("Correct! The (canonical) answer was ~w \n", [CanonicalAnswer]).

% score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, Score, ScoringRange) takes in one additional argument,
% ScoringRange, and uses that to give part scores to answers that are reasonably close to the actual answer using the following
% formula: Score = max(0, 1- abs(SubmittedAnswer - RealAnswer) / ScoringRange)
score_answer(UserAnswer, CanonicalAnswer, _, 0, ScoringRange) :-
  atom_number(UserAnswer, NumUserAnswer),
  number(CanonicalAnswer),
  Diff is abs(NumUserAnswer - CanonicalAnswer),
  Diff >= ScoringRange,
  format("You were off by over ~w! The answer was ~w \n", [ScoringRange, CanonicalAnswer]).

score_answer(UserAnswer, CanonicalAnswer, _, Score, ScoringRange) :- 
  atom_number(UserAnswer, NumUserAnswer),
  number(CanonicalAnswer),
  Diff is abs(NumUserAnswer - CanonicalAnswer),
  Diff < ScoringRange,
  Numerator is 1 - abs(NumUserAnswer - CanonicalAnswer),
  Score is max(0, 1 + Numerator / ScoringRange),
  format("Close! The (canonical) answer was ~w \n", [CanonicalAnswer]).

score_answer(_, CanonicalAnswer, _, 0, _) :-
  format("Incorrect! The answer was ~w \n", [CanonicalAnswer]).


% ask_and_score_questions/5 takes in a list of all rows for a quiz topic, the question format string, and the current score:
% It asks the user a question based off a random row, computes a further score, and repeats with further questions until RemainingQuestions becomes 0.
% MaxPossibleScore is currently the number of questions, but htis may change later
ask_and_score_questions(_, _, 0, Score, MaxPossibleScore, _) :-
  format("Your final score is: ~2f/~d!\n", [Score, MaxPossibleScore]).
ask_and_score_questions(AllRows, FormatString, RemainingQuestions, CurrentScore, MaxPossibleScore, ScoringRange) :-
  % Choose a random row from the list and unwrap it into the required bits
  random_member(Row, AllRows),
  Row = [_, LHSLabel, _, RHSLabel, RHSAltLabels],
  % Format the question (in a topic specific FormatString) and print it
  % (LHSLabel is the thing we're asking about, and RHSLabel + RHSAltLabel are the expected answers)
  format(atom(Question), FormatString, [LHSLabel]),
  writeln(""),
  writeln(Question),

  % Then read a user answer and compute a score for it. Currently, the score for each question is simply 1 if the
  % answer is correct and 0 otherwise
  read_line_to_string(user_input, UserAnswer),
  score_answer(UserAnswer, RHSLabel, RHSAltLabels, Score, ScoringRange),

  % Add to the score and print it
  NewScore is CurrentScore + Score,
  format("Your score so far is: ~2f/~d\n", [NewScore, MaxPossibleScore]),

  % Decrement the RemainingQuestions counter and recurse
  NewRemainingQuestions is RemainingQuestions - 1,
  ask_and_score_questions(AllRows, FormatString, NewRemainingQuestions, NewScore, MaxPossibleScore, ScoringRange).

log_cache_error(load, TopicFilename, Err) :- format('Error loading cache file ~w: ~w\n', [TopicFilename, Err]), fail.
log_cache_error(save, TopicFilename, Err) :- format('Error saving cache file ~w: ~w\n', [TopicFilename, Err]).

% load_topic_questions/2 loads questions from the cache, or loads them from Wikidata if there is no cached result
load_topic_questions(TopicFilename, OutputRows) :-
  catch(load_query_results(TopicFilename, OutputRows),
        error(Err, _Context),
        log_cache_error(load, TopicFilename, Err)),
  writeln("Loaded questions from cache.").
load_topic_questions(TopicFilename, OutputRows) :-
  writeln("Loading questions from Wikidata..."),
  get_all_from_rq_file(TopicFilename, OutputRows),
  writeln("Saving results to cache..."),
  catch(save_query_results(TopicFilename, OutputRows),
        error(Err, _Context),
        log_cache_error(save, TopicFilename, Err)).

% play_topic/1 takes in a quiz topic, loads data rows from Wikiedata, and starts the game
play_topic(quiz_topic(TopicFilename, TopicDescription, FormatString), RemainingQuestions, ScoringRange) :-
  format("Playing topic: ~w\n", [TopicDescription]),
  load_topic_questions(TopicFilename, AllRows),
  ask_and_score_questions(AllRows, FormatString, RemainingQuestions, 0, RemainingQuestions, ScoringRange).

% parse_or_prompt_topic/3 generates a quiz topic from user input, or
% calls choose_topic if no query file is passed in from the CLI args
parse_or_prompt_topic(QueryFile, QuestionFormatString, QuizTopic) :-
  ground(QueryFile),
  QuizTopic = quiz_topic(QueryFile, QueryFile, QuestionFormatString).
parse_or_prompt_topic(_, _, QuizTopic) :- choose_topic(QuizTopic).
