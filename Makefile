.PHONY: test
test:
	swipl -g run_tests -t halt test.pl

.PHONY: test-repl
test-repl:
	swipl test.pl
