.SUFFIXES: .el .elc
.PHONY: all clean

DIR := $(shell pwd)
SUBDIR := $(shell basename $(DIR))
HELPER := -l ~/.emacs.d/lisp/batch-helper

EMACS ?= emacs

LISP ?= $(wildcard *.el)
LISP := $(filter-out $(SUBDIR)-loaddefs.el,$(LISP))
ELCS ?= $(LISP:.el=.elc)

.el.elc:
	@echo Compile $<
	@$(EMACS) -batch -q $(HELPER) -f batch-byte-compile $<
