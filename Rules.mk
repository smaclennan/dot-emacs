.SUFFIXES: .el .elc
.PHONY: all clean

EMACS ?= emacs
LISP ?= $(wildcard *.el)
ELCS ?= $(LISP:.el=.elc)

# This needs to be absolute
HELPER ?= -l ~/.emacs.d/lisp/batch-helper

.el.elc:
	@echo Compile $<
	@$(EMACS) -batch -Q $(HELPER) -f batch-byte-compile $<

# To override this rule, put a rule: before including Rules.mk
all:	$(ELCS)

clean:
	rm -f *.elc TAGS *~ *loaddefs.el $(CLEAN)
