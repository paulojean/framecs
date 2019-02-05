EMACS ?= emacs
CASK ?= cask

.PHONY: all
all: unit-tests feature-tests

.PHONY: unit-tests
unit-tests:
	$(CASK) exec buttercup -L .

.PHONY: feature-tests
feature-tests:
	$(CASK) exec ecukes --no-win
