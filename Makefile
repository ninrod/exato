emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el
LIBS = -l test/elpa.el -l evil.el -l goto-chg.el -l undo-tree.el -L . -l exato.el -L test -l exato-test.el

update:
	$(emacs) -batch -l test/make-update.el

emacs:
	$(emacs) -Q $(LIBS) \
	--eval "(evil-mode 1)"

compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el

clean:
	rm -f *.elc; git clean -fdx

.PHONY: update compile test clean
