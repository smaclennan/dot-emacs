.SUFFIXES: .el .elc

first_rule: all

# When building inside of Emacs it seems EMACS=t
ifeq ($(EMACS),t)
EMACS=
endif

DIR := $(shell pwd)
SUBDIR := $(shell basename $(DIR))

ifeq ($(EMACS),)
ifneq ($(findstring .xemacs, $(DIR)),)
EMACS=xemacs
else
ifneq ($(findstring .sxemacs, $(DIR)),)
EMACS=sxemacs
else
EMACS=emacs
endif
endif
endif

ifeq ($(EMACS), emacs)
HELPER := -l ~/.emacs.d/esp/esp

$(SUBDIR)-loaddefs.el:
	@echo Create $(EMACS) $(SUBDIR)-loaddefs.el ...
	@$(EMACS) -batch -q $(HELPER) -l build-loaddefs.el -f build-loaddefs
endif

.el.elc:
	$(EMACS) -batch -q $(HELPER) -f batch-byte-compile $<
	@rm -f $(SUBDIR)-loaddefs.el
