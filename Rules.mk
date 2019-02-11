.SUFFIXES: .el .elc
.PHONY: all clean

DIR := $(shell pwd)
SUBDIR := $(shell basename $(DIR))
HELPER := -l ~/.emacs.d/lisp/batch-helper
LOADFILE := $(SUBDIR)-loaddefs.el

EMACS ?= emacs

LISP ?= $(wildcard *.el)
LISP := $(filter-out $(LOADFILE),$(LISP))
ELCS ?= $(LISP:.el=.elc)

.el.elc:
	@echo Compile $(PDIR)$<
	@$(EMACS) -batch -Q $(HELPER) -f batch-byte-compile $<
