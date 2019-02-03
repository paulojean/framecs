EMACS ?= emacs
CASK ?= cask

.PHONY: all
all: unit feature

.PHONY: unit
unit:
	$(CASK) exec buttercup -L .

.PHONY: feature
feature:
	$(CASK) exec ecukes --no-win
