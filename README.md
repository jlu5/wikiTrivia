# wikiTrivia

**wikiTrivia** is an interactive CLI trivia game written in [SWI-Prolog](https://www.swi-prolog.org/). It sources questions from [Wikidata](https://wikidata.org), using swipl's built-in [SPARQL](https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial) client to pull down results.

This project was written as part of [CPSC 312 - Functional and Logic Programming](https://steven-wolfman.github.io/cpsc-312-website/) @ The University of British Columbia, Sep-Dec 2021.

## Features

- Alternative answers (usually alt labels in Wikidata)
- Hints for text responses - 3 attempts per question
- Lenient scoring for numerical answers, using a range of accepted responses
- Caching to local JSON
  - Due to encoding issues, this does not work on Windows yet: see https://swi-prolog.discourse.group/t/1896

### Query format

All of the SPARQL queries used in the game expect a specific set of arguments in order: the question entity, the question entity's label, the answer entity/value, the answer entity/value's label, and any alternative text answers. For values, the game currently supports text literals (with or without a language), decimal literals, and date literals (exported as the year only).

In practice, trivia queries should take care to filter away extraneous relations for playability - e.g. [`capitals-to-countries.rq`](queries/capitals-to-countries.rq) has to remove historical countries or those that a particular city was *formerly* in.

See `queries/` for more examples.

## Usage

You can start the game by running `./main.pl`

The following command line arguments are supported:

```
wikiTrivia - a trivia quiz game based off Wikidata relations
--num-rounds       -n  integer=5                  number of rounds to play
--scoring-range    -R  integer=100                range size of accepted answers for
                                                    numerical based scoring
--query-file       -f  FILE:atom=_                load a SPARQL query from FILE instead of
                                                    prompting
--question-format  -q  QFORMAT:atom=Question: ~w  string to format questions; this should
                                                    include a ~w to substitute the question
                                                    item
--help             -h  boolean=false              show this help text
```

Tests use PlUnit; you can run them with `make test` or `make test-repl`.
