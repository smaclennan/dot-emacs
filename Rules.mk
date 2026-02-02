.SUFFIXES: .el .elc
.PHONY: all clean

DIR := $(shell pwd)
SUBDIR := $(shell basename $(DIR))
# This needs to be absolute
HELPER := -l ~/.emacs.d/lisp/batch-helper
LOADFILE := $(SUBDIR)-loaddefs.el

EMACS ?= emacs

# SAM LISP ?= $(filter-out $(LOADFILE),$(wildcard *.el))
LISP ?= $(wildcard *.el)
ELCS ?= $(LISP:.el=.elc)

.el.elc:
	@echo Compile $(SUBDIR)/$<
	@$(EMACS) -batch -Q $(HELPER) -f batch-byte-compile $<

# To override this rule, put a rule: before including Rules.mk
all:	$(ELCS)

loadfile: $(LOADFILE)

$(LOADFILE): $(LISP)
	@echo "Update  $(LOADFILE)"
	@$(EMACS) -batch -Q $(HELPER) -f update-loadfile $(LOADFILE)

clean:
	rm -f *.elc TAGS *~ loaddefs.el $(CLEAN)
