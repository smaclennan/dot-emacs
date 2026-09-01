.PHONY: all clean
.SUFFIXES: .el .elc

EMACS ?= emacs
LISP ?= $(wildcard *.el)
ELCS ?= $(LISP:.el=.elc)

# Need to add lisp dir to load-path
HELPER = --eval='(add-to-list (quote load-path) (concat user-emacs-directory "lisp"))'

.el.elc:
	@echo Compile $<
	@$(EMACS) -batch -Q $(HELPER) -f batch-byte-compile $<

all:	$(ELCS)

clean:
	rm -f *.elc TAGS *~ *loaddefs.el
