.PHONY: all elcs clean

ifeq ($(EMACS),)
BASE=$(shell basename $(PWD))
ifeq ($(BASE),.sxemacs)
EMACS := sxemacs
else ifeq ($(BASE),.xemacs)
EMACS := xemacs
else
EMACS := emacs
endif
endif

all:
ifeq ($(EMACS),emacs)
	make EMACS=$(EMACS) -C esp
endif
	make EMACS=$(EMACS) -C site-packages/lisp

elcs:
ifeq ($(EMACS),emacs)
	make EMACS=$(EMACS) -C esp elcs
endif
	make EMACS=$(EMACS) -C site-packages/lisp elcs

clean:
	@echo Clean $(EMACS) ...
	rm -f *.elc
	make -C site-packages/lisp EMACS=$(EMACS) clean
	make -C esp EMACS=$(EMACS) clean

clean-sam:
	make -C site-packages/lisp/sam clean
