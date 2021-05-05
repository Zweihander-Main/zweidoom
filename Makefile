LINT_FILES :=
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')

.PHONY: test

test:
	cask exec buttercup -L .

testWatch:
	while true; do \
		make test; \
		inotifywait -qre close_write $(ELISP_FILES); \
	done

lint:
	cask exec elsa $(LINT_FILES)
