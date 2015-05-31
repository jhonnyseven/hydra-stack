LIBS = -L hydra -L hydra-stack

test:
	emacs --batch $(LIBS) -nw -l hydra-stack-test.el $(TESTS) -f ert-run-tests-batch-and-exit
