
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
  quiz_topic("queries/billion-euro-companies.rq", "Companies worth a billion euros... and where they're from", "What country is ~w based in?"),
  quiz_topic("queries/national-cuisines.rq", "National cuisines", "What country makes ~w?"),
  % Enable these once we have a more lenient way of scoring numerical answers!
  quiz_topic("queries/inception-vancouver-orgs.rq", "Vancouver-based organizations and what year they were founded", "When was ~w founded?"),
  quiz_topic("queries/altitude-major-cities.rq", "Major cities: elevation", "Altitude of ~w (metres)?"),
  quiz_topic("queries/inception-major-cities.rq", "Major cities: founding year", "What year was ~w founded?"),
  quiz_topic("queries/inception-oses-programming-langs.rq", "Birth years of programming langs and operating systems", "When was ~w created?")
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
  length(AllTopics, NumTopics),
  format("Choose a topic by entering its number (1 to ~w):\n", [NumTopics]),
  print_known_topics(AllTopics, 1),
  read_line_to_string(user_input, InputString),
  number_string(Index, InputString),
  nth1(Index, AllTopics, QuizTopic).
choose_topic(QuizTopic) :-
  writeln("Invalid input"),
  choose_topic(QuizTopic).

% determine_question_type/2
% tests if its a number based question (gives path 1)
determine_question_type(CanonicalAnswer, 1) :- 
  number(CanonicalAnswer). 

% default word based question (gives path 0)
determine_question_type(_, 0). 

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
  NumTries > 0, 
  write(Head),
  write(" "),  
  LettersLeft is Length - 1, 
  TriesLeft is NumTries - 1, 
  build_hint(Rest, LettersLeft, TriesLeft). 

% give_hint/2
% when called gives hint and breaks up CanonicalAnswer into a char list
% NumTries Indicates the number of attempts the user has done as a guess, and is used to see how much of the letter should be revealed. 
give_hint(CanonicalAnswer, NumTries) :-
  string_length(CanonicalAnswer, Length), 
  string_chars(CanonicalAnswer, Chars), 
  write("HINT: "), 
  build_hint(Chars, Length, NumTries).

% text_score_answer/6 
% This path is indicated as 1 in index parameter 5. This path is in the case where the player guesses a wrong answer. 
text_score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, 1, NumTries) :-
  AcceptableAnswers = [CanonicalAnswer|AlternativeAnswers],
  % First convert input and accepted answers to lowercase so that matches are case insensitive
  string_lower(UserAnswer, NormalizedUserAnswer),
  maplist(string_lower, AcceptableAnswers, NormalizedAcceptableAnswers),
  \+ member(NormalizedUserAnswer, NormalizedAcceptableAnswers), 
  writeln(""), 
  give_hint(CanonicalAnswer, NumTries).

% text_score_answer/6 
% This path is indicated as 1 in index parameter 5. This path is in the case where the player guesses the right answer. 
text_score_answer(UserAnswer, CanonicalAnswer, AlternativeAnswers, 2, _) :-
  AcceptableAnswers = [CanonicalAnswer|AlternativeAnswers],
  % First convert input and accepted answers to lowercase so that matches are case insensitive
  string_lower(UserAnswer, NormalizedUserAnswer),
  maplist(string_lower, AcceptableAnswers, NormalizedAcceptableAnswers),
  member(NormalizedUserAnswer, NormalizedAcceptableAnswers).

% a bound checker for comparing NumTries and Current Path of Loop Current
% if the current NumTries is 2 and Current Path is 1 (Wrong) then apply offset 1
check_num_tries(2, 1, 1). 
check_num_tries(_, _, 0). 

% loop_current_question/7
% Default case when the player should run out of Chances
loop_current_question(Row, _, _, -1, 0, _, _) :- 
  Row = [_, _, _, RHSLabel, _],
  writeln("Out of Chances!"),
  format("The (canonical) answer was ~w \n", [RHSLabel]).

% loop_current_question/7
% Number of Hints = StartingNumChances - 1
% if answer is found, FoundAnswer = 2, else if wrong FoundAnswer = 1
% FinalScore determines the score to be given back for the current question. 
loop_current_question(Row, FormatString, CurrentScore, NumChances, FinalScore, MaxChances, 1) :-
  % Format the question (in a topic specific FormatString) and print it
  % (LHSLabel is the thing we're asking about, and RHSLabel + RHSAltLabel are the expected answers)
  Row = [_, LHSLabel, _, RHSLabel, RHSAltLabels],
  format(atom(Question), FormatString, [LHSLabel]), 
  writeln(""), 
  write(Question),
  NumTries is MaxChances - NumChances, 
  % Tells player the number of chances they have left before they lose all the points for this question
  format(" Number of Chances Left: ~d\n", NumChances),
  read_line_to_string(user_input, UserAnswer),
  text_score_answer(UserAnswer, RHSLabel, RHSAltLabels, FoundAnswer, NumTries), 
  % offsets for recursion
  check_num_tries(NumTries, FoundAnswer, Offset), 
  RemainingChances is NumChances - 1 - Offset,
  NewScore is CurrentScore / 2, 
  loop_current_question(Row, FormatString, NewScore, RemainingChances, FinalScore, MaxChances, FoundAnswer). 

loop_current_question(Row, _, CurrentScore, _, FinalScore, _, 2) :-
  Row = [_, _, _, RHSLabel, _],
  FinalScore is CurrentScore * 2, 
  writeln("Good Work! "),
  format("The (canonical) answer was ~w \n", [RHSLabel]).

% ask_and_score/8
% prints out final score after the game is finished
ask_and_score(_, _, _, 0, Score, MaxPossibleScore, _, _) :-
  format("\nYour final score is: ~2f/~d!\n", [Score, MaxPossibleScore]).

% for text based questions
% Path 0 (Index 7) for the 3 Chances loop 
ask_and_score(AllRows, Row, FormatString, RemainingQuestions, CurrentScore, MaxPossibleScore, ScoringRange, 0) :-
  % start loop to give players a chance to answer with a couple given hints
  % Row, FormatString, CurrentScore = 1 (Max possible points), NumChances = 3, Score is given back, MaxChances = 3, 1 (Starting Path)
  loop_current_question(Row, FormatString, 1, 3, Score, 3, 1),
  % Add to the score and print it
  NewScore is CurrentScore + Score,
  format("Your score so far is: ~2f/~d\n", [NewScore, MaxPossibleScore]),

  % Decrement the RemainingQuestions counter and recurse
  NewRemainingQuestions is RemainingQuestions - 1,
  grab_questions(AllRows, FormatString, NewRemainingQuestions, NewScore, MaxPossibleScore, ScoringRange).

% for number based questions
% Path 1 (Index 7) for Questions With Scoring Range 
ask_and_score(AllRows, Row, FormatString, RemainingQuestions, CurrentScore, MaxPossibleScore, ScoringRange, 1) :-
  Row = [_, LHSLabel, _, RHSLabel, RHSAltLabels],
  % Format the question (in a topic specific FormatString) and print it
  % (LHSLabel is the thing we're asking about, and RHSLabel + RHSAltLabel are the expected answers)
  format(atom(Question), FormatString, [LHSLabel]),
  writeln(""),
  writeln(Question),

  % Then read a user answer and compute a score for it
  read_line_to_string(user_input, UserAnswer),
  score_answer(UserAnswer, RHSLabel, RHSAltLabels, Score, ScoringRange),

  % Add to the score and print it
  NewScore is CurrentScore + Score,
  format("Your score so far is: ~2f/~d\n", [NewScore, MaxPossibleScore]),

  % Decrement the RemainingQuestions counter and recurse
  NewRemainingQuestions is RemainingQuestions - 1,
  grab_questions(AllRows, FormatString, NewRemainingQuestions, NewScore, MaxPossibleScore, ScoringRange).

% grab_questions/6
% Choose a random question from AllRows. 
% Depending on Type, the call to ask_and_score is decided. 
grab_questions(AllRows, FormatString, RemainingQuestions, CurrentScore, MaxPossibleScore, ScoringRange) :-
   % Choose a random row from the list and unwrap it into the required bits
  random_member(Row, AllRows),
  Row = [_, _, _, RHSLabel, _],
  determine_question_type(RHSLabel, Type), 
  ask_and_score(AllRows, Row, FormatString, RemainingQuestions, CurrentScore, MaxPossibleScore, ScoringRange, Type). 

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
  Score is max(0, 1 - Diff / ScoringRange),
  format("Close! The (canonical) answer was ~w \n", [CanonicalAnswer]).

score_answer(_, CanonicalAnswer, _, 0, _) :-
  format("Incorrect! The answer was ~w \n", [CanonicalAnswer]).


% ask_and_score_questions/6 takes in a list of all rows for a quiz topic, the question format string, and the current score:
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
  grab_questions(AllRows, FormatString, RemainingQuestions, 0, RemainingQuestions, ScoringRange).

% parse_or_prompt_topic/3 generates a quiz topic from user input, or
% calls choose_topic if no query file is passed in from the CLI args
parse_or_prompt_topic(QueryFile, QuestionFormatString, QuizTopic) :-
  ground(QueryFile),
  QuizTopic = quiz_topic(QueryFile, QueryFile, QuestionFormatString).
parse_or_prompt_topic(_, _, QuizTopic) :- choose_topic(QuizTopic).
