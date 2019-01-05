.SUFFIXES: .el .elc
.PHONY: first-rule

first-rule: all

DIR := $(shell pwd)
SUBDIR := $(shell basename $(DIR))

EMACS ?= emacs

ifeq ($(LISP),)
LISP := $(wildcard *.el)
endif

# GNU Emacs

HELPER := -l ~/.emacs.d/lisp/helper

$(SUBDIR)-loaddefs.el:
	@echo Create $(EMACS) $(SUBDIR)-loaddefs.el ...
	@$(EMACS) -batch -q $(HELPER) -l build-loaddefs.el -f build-loaddefs

LOAD_FILES = $(SUBDIR)-loaddefs.el

LISP := $(filter-out $(SUBDIR)-loaddefs.el,$(LISP))

ifeq ($(ELCS),)
ELCS = $(LISP:.el=.elc)
endif

.el.elc:
	$(EMACS) -batch -q $(HELPER) -f batch-byte-compile $<
	@rm -f $(LOAD_FILES)
